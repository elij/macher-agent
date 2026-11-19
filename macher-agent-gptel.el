;;; macher-agent-gptel.el --- Clean gptel boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Clean gptel boundary implementation for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'rx)
(eval-and-compile
  (require 'gv))
(require 'gptel)
(require 'macher-agent-core)
(require 'macher-agent-presets)

(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function vc-root-dir "vc-hooks" ())
(declare-function gptel--create-prompt-buffer "gptel-request" (&optional prompt-end))
(declare-function gptel--parse-buffer "gptel-request" (backend max-entries))
(declare-function gptel--parse-list "gptel-request" (backend prompt-list))
(declare-function gptel--parse-list-and-insert "gptel-request" (prompts))
(declare-function gptel--model-request-params "gptel-request" (model))
(declare-function gptel--error-p "gptel-request" (info))
(declare-function gptel--request-data "gptel-request" (backend prompts))
(declare-function gptel--inject-prompt "gptel-request" (backend data new-prompt &optional position))
(declare-function gptel--json-encode "gptel-request" (object))
(declare-function gptel--insert-response "gptel" (response info &optional raw))
(declare-function gptel-curl--stream-insert-response "gptel" (response info &optional raw))
(declare-function gptel-prompt-prefix-string "gptel" ())
(declare-function gptel-response-prefix-string "gptel" ())
(declare-function gptel-context--collect-media "gptel-context" ())

(defvar gptel-system-prompt)
(defvar gptel-directives)
(defvar gptel--preset)
(defvar gptel--bounds)
(defvar gptel--tool-names)
(defvar gptel--backend-name)
(defvar gptel--known-presets)
(defvar gptel--known-tools)
(defvar gptel--known-backends)
(defvar gptel--num-messages-to-send)
(defvar gptel-track-response)
(defvar gptel-save-state-hook)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)
(defvar gptel-include-reasoning)
(defvar gptel-include-tool-results)
(defvar gptel-context)
(defvar gptel-use-context)
(defvar gptel-stream)
(defvar gptel-use-curl)
(defvar gptel-use-tools)
(defvar gptel-tools)
(defvar gptel-temperature)
(defvar gptel-max-tokens)
(defvar gptel-cache)
(defvar gptel--request-params)

(defvar macher-agent-wire-pre-truncate-hook nil
  "Hook run with `(ORIG-BUF SHADOW-OFFSET)' before wire horizon truncation.")

(defvar macher-agent-wire-invalidated-hook nil
  "Hook run with `(ORIG-BUF REASON)' when shadow wire spans are invalidated.")

(defvar-local macher-agent--wire-backend nil
  "Backend instance associated with committed spans in the shadow wire buffer.")
(put 'macher-agent--wire-backend 'permanent-local t)

(defvar-local macher-agent--wire-span-counter 0
  "Monotonic span counter inside a shadow wire buffer.")
(put 'macher-agent--wire-span-counter 'permanent-local t)

(defvar-local macher-agent--wire-last-media-hash nil
  "Hash of `gptel-context' media already injected into cached wire history.")
(put 'macher-agent--wire-last-media-hash 'permanent-local t)

(defvar-local macher-agent--is-restored-session nil
  "Track whether current buffer is restored from a saved state.")

(defvar-local macher-agent--is-workspace nil
  "Flag whether current buffer is initialised as a Macher Agent workspace.")
(put 'macher-agent--is-workspace 'permanent-local t)

(cl-defstruct macher-agent-transmission-state
  "Transmission pipeline state for compiling a gptel request."
  target-buffer
  presets
  skills
  redirected-skill
  backend
  model
  temperature
  max-tokens
  request-params
  disable-local-tools
  tools
  ptc-primitives
  known-presets
  base-prompt
  directives
  compiled-prompt
  redirect-prompt
  context
  plugins
  (consume-boot-directive t))

(defcustom macher-agent-prompt-transformers
  '(macher-agent-transformer-deduplicate-tools)
  "List of transformer functions to clean buffer prior to transmission."
  :type '(repeat function)
  :group 'macher-agent)

(defcustom macher-agent-max-duplicate-tools 2
  "Maximum number of identical tool calls to retain in context history."
  :type 'integer
  :group 'macher-agent)

(defun macher-agent--task-submitted-p (info)
  "Return non-nil when FSM INFO buffer holds a staged `submit_task_result'."
  (when-let* ((buf (and (macher-agent--plist-p info) (plist-get info :buffer)))
              ((buffer-live-p buf))
              ((buffer-local-value 'macher-agent--staged-submission buf)))
    (plist-put info :tool-use nil)
    (plist-put info :tool-result nil)
    t))

(defun macher-agent--sanitize-tool-result-entries (tool-results info)
  "Ensure each `(TOOL ARGS RESULT)' in TOOL-RESULTS holds a `gptel-tool'.
Correlates nil tool slots against `:tool-use' in FSM INFO."
  (let ((tool-use (and (macher-agent--plist-p info) (plist-get info :tool-use))))
    (mapcar
     (lambda (entry)
       (if (or (not (consp entry)) (gptel-tool-p (car entry)))
           entry
         (let* ((args (nth 1 entry))
                (result (nth 2 entry))
                (matched-tu
                 (or (cl-find-if (lambda (tu)
                                   (and (equal (plist-get tu :args) args)
                                        (equal (plist-get tu :result) result)))
                                 tool-use)
                     (cl-find-if (lambda (tu)
                                   (equal (plist-get tu :result) result))
                                 tool-use)
                     (car tool-use)))
                (tname (or (and (listp matched-tu) (plist-get matched-tu :name))
                           "unknown_tool")))
           (list (gptel-make-tool
                  :name (format "%s" tname)
                  :description "Blocked or unavailable tool"
                  :function #'ignore
                  :include t)
                 args
                 result))))
     tool-results)))

(defun macher-agent--sanitize-fsm-tool-results (fsm)
  "Sanitise `:tools' and `:tool-result' on FSM before `TRET' display handlers."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let ((info (gptel-fsm-info fsm)))
      (when (macher-agent--plist-p info)
        (let ((tools (plist-get info :tools))
              (tool-results (plist-get info :tool-result))
              (updated nil))
          (when (and (listp tools) (cl-some (lambda (tl) (not (gptel-tool-p tl))) tools))
            (setq info (plist-put (copy-sequence info) :tools
                                  (cl-remove-if-not #'gptel-tool-p tools))
                  updated t))
          (when (and (listp tool-results)
                     (cl-some (lambda (tr) (not (gptel-tool-p (car-safe tr)))) tool-results))
            (unless updated (setq info (copy-sequence info)))
            (setq info (plist-put info :tool-result
                                  (macher-agent--sanitize-tool-result-entries tool-results info))
                  updated t))
          (when updated
            (macher-agent--set-fsm-info fsm info)
            (setf (gptel-fsm-info fsm) info)))))))

(defun macher-agent--install-tret-done-edge (fsm)
  "Ensure transition table for FSM routes `TRET' to `DONE' on staged submit."
  (when (and fsm (fboundp 'gptel-fsm-table))
    (let* ((orig-table (gptel-fsm-table fsm))
           (tret-cell (assq 'TRET orig-table)))
      (when tret-cell
        (let ((tret-edges (copy-alist (cdr tret-cell))))
          (unless (assq #'macher-agent--task-submitted-p tret-edges)
            (let* ((err-edge (cl-find-if (lambda (e) (eq (cdr e) 'ERRS)) tret-edges))
                   (rest-edges (if err-edge (remove err-edge tret-edges) tret-edges))
                   (new-edges (if err-edge
                                  (cons err-edge
                                        (cons (cons #'macher-agent--task-submitted-p 'DONE)
                                              rest-edges))
                                (cons (cons #'macher-agent--task-submitted-p 'DONE)
                                      rest-edges)))
                   (new-table (copy-alist orig-table)))
              (setf (alist-get 'TRET new-table) new-edges)
              (setf (gptel-fsm-table fsm) new-table))))))))

;;; Shadow Wire Buffer Core Helpers

(defun macher-agent--wire-enabled-p (buf &optional info)
  "Return non-nil when shadow wire caching is enabled for BUF and FSM INFO."
  (and macher-agent-wire-buffer-mode
       (bufferp buf)
       (buffer-live-p buf)
       (or (buffer-local-value 'macher-agent--is-workspace buf)
           (and (boundp 'macher-agent-mode)
                (buffer-local-value 'macher-agent-mode buf))
           (and (macher-agent--plist-p info)
                (macher-agent-valid-context-p
                 (or (plist-get info :macher-agent-context)
                     (plist-get info :context))))
           (macher-agent-valid-context-p
            (buffer-local-value 'macher-agent--persistent-context buf)))))

(defun macher-agent--deep-copy-wire-data (obj)
  "Return an immutable deep copy of wire structure OBJ without text properties."
  (cond
   ((stringp obj) (substring-no-properties obj))
   ((vectorp obj)
    (let* ((len (length obj))
           (vec (make-vector len nil)))
      (dotimes (i len)
        (aset vec i (macher-agent--deep-copy-wire-data (aref obj i))))
      vec))
   ((consp obj)
    (let ((head nil)
          (cur obj))
      (while (consp cur)
        (push (macher-agent--deep-copy-wire-data (car cur)) head)
        (setq cur (cdr cur)))
      (let ((tail (macher-agent--deep-copy-wire-data cur)))
        (dolist (elt head tail)
          (setq tail (cons elt tail))))))
   (t obj)))

(defun macher-agent--wire-estimate-chars (entries)
  "Estimate JSON character footprint of wire ENTRIES."
  (cond
   ((null entries) 0)
   ((fboundp 'gptel--json-encode)
    (condition-case nil
        (length (gptel--json-encode (vconcat entries)))
      (error (length (prin1-to-string entries)))))
   (t (length (prin1-to-string entries)))))

(defun macher-agent--wire-normalize-text (str &optional prompt-pfx response-pfx)
  "Return whitespace-normalised canonical form of STR.
Strips optional PROMPT-PFX and RESPONSE-PFX before collapsing whitespace."
  (if (not (stringp str))
      ""
    (let ((s (string-trim str)))
      (dolist (pfx (list prompt-pfx response-pfx))
        (when (and (stringp pfx)
                   (not (string-empty-p (string-trim pfx)))
                   (string-prefix-p (string-trim pfx) s))
          (setq s (string-trim-left (substring s (length (string-trim pfx)))))))
      (when (string-match "\\`\\(?:#+\\|\\*+\\|>+\\)[ \t]+" s)
        (setq s (substring s (match-end 0))))
      (string-trim (replace-regexp-in-string "[ \t\r\n]+" " " s)))))

(defun macher-agent--wire-norm-hash (buf beg end)
  "Compute normalised content hash of BUF between BEG and END."
  (if (not (and (bufferp buf) (buffer-live-p buf)))
      (sxhash-equal "")
    (with-current-buffer buf
      (let* ((b (max (point-min) (min (point-max) (if (markerp beg) (marker-position beg) beg))))
             (e (max (point-min) (min (point-max) (if (markerp end) (marker-position end) end))))
             (raw (if (< b e) (buffer-substring-no-properties b e) ""))
             (ppfx (when (fboundp 'gptel-prompt-prefix-string)
                     (ignore-errors (gptel-prompt-prefix-string))))
             (rpfx (when (fboundp 'gptel-response-prefix-string)
                     (ignore-errors (gptel-response-prefix-string)))))
        (sxhash-equal (macher-agent--wire-normalize-text raw ppfx rpfx))))))

(defun macher-agent--wire-last-committed-end (buf &optional before-pos)
  "Return largest committed `macher-agent-orig-end' in BUF at or before BEFORE-POS."
  (when-let* ((wbuf (macher-agent--wire-buffer-for buf nil)))
    (let ((limit (or before-pos (with-current-buffer buf (point-max))))
          (dead-turns (make-hash-table :test 'equal))
          (best nil))
      (with-current-buffer wbuf
        (let ((pos (point-min)))
          (while (< pos (point-max))
            (let ((next (next-single-property-change
                         pos 'macher-agent-span-id wbuf (point-max)))
                  (tid (get-text-property pos 'macher-agent-turn-id))
                  (kind (get-text-property pos 'macher-agent-span-kind))
                  (end-m (get-text-property pos 'macher-agent-orig-end))
                  (beg-m (get-text-property pos 'macher-agent-orig-beg))
                  (nonempty (get-text-property pos 'macher-agent-orig-nonempty)))
              (when (and tid (memq kind '(user-prompt gap))
                         nonempty
                         (or (not (and (markerp beg-m) (markerp end-m)
                                       (eq (marker-buffer beg-m) buf)
                                       (eq (marker-buffer end-m) buf)))
                             (>= (marker-position beg-m) (marker-position end-m))))
                (puthash tid t dead-turns))
              (when (and (not (and tid (gethash tid dead-turns)))
                         (markerp end-m)
                         (eq (marker-buffer end-m) buf)
                         (<= (marker-position end-m) limit)
                         (or (not nonempty)
                             (and (markerp beg-m)
                                  (< (marker-position beg-m) (marker-position end-m)))))
                (let ((ep (marker-position end-m)))
                  (when (or (null best) (> ep best))
                    (setq best ep))))
              (setq pos next)))))
      best)))

(defun macher-agent--shadow-close-reasoning-fence (wbuf fsm)
  "Close an open reasoning fence in shadow buffer WBUF for FSM."
  (when (and fsm (bufferp wbuf) (buffer-live-p wbuf))
    (let ((info (gptel-fsm-info fsm)))
      (when (and (macher-agent--plist-p info)
                 (plist-get info :macher-agent-shadow-in-reasoning))
        (with-current-buffer wbuf
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t)
                (pt (point-max)))
            (save-excursion
              (goto-char pt)
              (unless (eq (char-before) ?\n)
                (insert "\n"))
              (insert "```\n\n")
              (put-text-property pt (point) 'gptel 'ignore))))
        (setq info (plist-put info :macher-agent-shadow-in-reasoning nil))
        (macher-agent--set-fsm-info fsm info)))))

(defun macher-agent--shadow-append-reasoning (wbuf fsm chunk)
  "Append reasoning CHUNK into shadow buffer WBUF for FSM."
  (when (and (bufferp wbuf) (buffer-live-p wbuf))
    (if (eq chunk t)
        (macher-agent--shadow-close-reasoning-fence wbuf fsm)
      (when (and (stringp chunk) (not (string-empty-p chunk)))
        (let ((info (and fsm (gptel-fsm-info fsm))))
          (with-current-buffer wbuf
            (let ((inhibit-read-only t)
                  (inhibit-modification-hooks t)
                  (pt (point-max)))
              (save-excursion
                (goto-char pt)
                (unless (and (macher-agent--plist-p info)
                             (plist-get info :macher-agent-shadow-in-reasoning))
                  (unless (or (bobp) (eq (char-before) ?\n))
                    (insert "\n"))
                  (insert "``` reasoning\n")
                  (when (macher-agent--plist-p info)
                    (setq info (plist-put info :macher-agent-shadow-in-reasoning t))
                    (macher-agent--set-fsm-info fsm info)))
                (insert (substring-no-properties chunk))
                (put-text-property pt (point) 'gptel 'ignore)))))))))

(defun macher-agent--shadow-append-tool-results (wbuf fsm tool-results info)
  "Append formatted TOOL-RESULTS into shadow buffer WBUF for FSM using INFO."
  (when (and (bufferp wbuf) (buffer-live-p wbuf) (listp tool-results) tool-results)
    (macher-agent--shadow-close-reasoning-fence wbuf fsm)
    (let ((tool-use (and (macher-agent--plist-p info) (plist-get info :tool-use)))
          (used-ids (make-hash-table :test 'equal)))
      (with-current-buffer wbuf
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (save-excursion
            (goto-char (point-max))
            (pcase-dolist (`(,tool ,args ,result) tool-results)
              (let* ((name (or (and (gptel-tool-p tool) (gptel-tool-name tool))
                               (macher-agent-canonical-tool-name tool)
                               "unknown_tool"))
                     (matched-tu
                      (or (cl-find-if
                           (lambda (tu)
                             (and (equal (plist-get tu :name) name)
                                  (equal (plist-get tu :args) args)
                                  (not (and (plist-get tu :id)
                                            (gethash (plist-get tu :id) used-ids)))))
                           tool-use)
                          (cl-find-if
                           (lambda (tu)
                             (and (equal (plist-get tu :name) name)
                                  (not (and (plist-get tu :id)
                                            (gethash (plist-get tu :id) used-ids)))))
                           tool-use)))
                     (id (and (listp matched-tu) (plist-get matched-tu :id)))
                     (call-str (prin1-to-string `(:name ,name :args ,args)))
                     (res-str (if (stringp result)
                                  (substring-no-properties result)
                                (format "%s" (or result ""))))
                     (head-str (truncate-string-to-width
                                (string-replace "\n" " " (format "(%s %S)" name args))
                                80 0 nil " ...)"))
                     (f1-beg nil)
                     (body-beg nil)
                     (f2-beg nil))
                (when id (puthash id t used-ids))
                (unless (or (bobp) (eq (char-before) ?\n))
                  (insert "\n"))
                (setq f1-beg (point))
                (insert (format "``` tool %s" head-str))
                (put-text-property f1-beg (point) 'gptel 'ignore)
                (setq body-beg (point))
                (insert "\n" call-str "\n\n" res-str)
                (put-text-property body-beg (point) 'gptel (cons 'tool id))
                (setq f2-beg (point))
                (insert "\n```\n\n")
                (put-text-property f2-beg (point) 'gptel 'ignore)))
            (when (and fsm (macher-agent--plist-p info))
              (let* ((cur-info (gptel-fsm-info fsm))
                     (cur-data (and (macher-agent--plist-p cur-info)
                                    (plist-get cur-info :data)))
                     (post-tret-len (length (macher-agent--existing-prompts-from-data cur-data))))
                (setq cur-info (plist-put cur-info :macher-agent-wire-shadow-tools-end (point)))
                (setq cur-info (plist-put cur-info :macher-agent-wire-shadow-final-written nil))
                (setq cur-info (plist-put cur-info :macher-agent-wire-wait-len post-tret-len))
                (macher-agent--set-fsm-info fsm cur-info)))))))))

(defun macher-agent--shadow-append-response-chunk (wbuf fsm text)
  "Append assistant response TEXT into shadow buffer WBUF for FSM."
  (when (and (bufferp wbuf) (buffer-live-p wbuf)
             (stringp text) (not (string-empty-p text)))
    (macher-agent--shadow-close-reasoning-fence wbuf fsm)
    (with-current-buffer wbuf
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)
            (pt (point-max)))
        (save-excursion
          (goto-char pt)
          (insert (substring-no-properties text))
          (put-text-property pt (point) 'gptel 'response))))
    (when fsm
      (let ((cur-info (gptel-fsm-info fsm)))
        (when (macher-agent--plist-p cur-info)
          (setq cur-info (plist-put cur-info :macher-agent-wire-shadow-final-written t))
          (macher-agent--set-fsm-info fsm cur-info))))))

(defun macher-agent--make-wrapped-fsm-callback (actual-fsm target-buf orig-cb)
  "Build wrapped FSM callback for ACTUAL-FSM, TARGET-BUF, and ORIG-CB."
  (lambda (&rest cb-args)
    (when (and target-buf
               (buffer-live-p target-buf)
               (fboundp 'gptel-fsm-state)
               (memq (gptel-fsm-state actual-fsm) '(DONE ERRS ABRT))
               (eq (buffer-local-value 'macher-agent--active-fsm target-buf)
                   actual-fsm))
      (with-current-buffer target-buf
        (setq-local macher-agent--active-fsm nil)))
    (let* ((first-arg (car cb-args))
           (cb-info (cadr cb-args))
           (fsm-info (and actual-fsm (fboundp 'gptel-fsm-info)
                          (ignore-errors (gptel-fsm-info actual-fsm))))
           (eff-info (if (macher-agent--plist-p fsm-info) fsm-info cb-info))
           (wire-active (and target-buf
                             (macher-agent--wire-enabled-p target-buf eff-info)
                             (macher-agent--plist-p eff-info)
                             (plist-get eff-info :macher-agent-turn-started)
                             (not (plist-get eff-info :macher-agent-wire-turn-invalidated))))
           (wbuf (when wire-active
                   (macher-agent--wire-buffer-for target-buf t))))
      (cond
       ((and (consp first-arg) (eq (car first-arg) 'reasoning))
        (when wbuf
          (macher-agent--shadow-append-reasoning wbuf actual-fsm (cdr first-arg)))
        (when (or (not wire-active)
                  (and (buffer-live-p target-buf)
                       (with-current-buffer target-buf
                         (bound-and-true-p gptel-include-reasoning))))
          (apply orig-cb cb-args)))
       ((and (consp first-arg) (eq (car first-arg) 'tool-result))
        (let* ((raw-tr (cdr first-arg))
               (cb-tools (and (macher-agent--plist-p cb-info)
                              (plist-get cb-info :tools))))
          (when (and (listp cb-tools)
                     (cl-some (lambda (tl) (not (gptel-tool-p tl))) cb-tools))
            (setq cb-info (plist-put (copy-sequence cb-info) :tools
                                     (cl-remove-if-not #'gptel-tool-p cb-tools)))
            (setcar (cdr cb-args) cb-info))
          (when (and (listp raw-tr)
                     (cl-some (lambda (tr) (not (gptel-tool-p (car-safe tr)))) raw-tr))
            (setq raw-tr (macher-agent--sanitize-tool-result-entries raw-tr cb-info))
            (setcar cb-args (cons 'tool-result raw-tr)))
          (when wbuf
            (macher-agent--shadow-append-tool-results wbuf actual-fsm raw-tr cb-info))
          (apply orig-cb cb-args)
          (when (and wire-active actual-fsm (buffer-live-p target-buf))
            (let* ((cur-info (gptel-fsm-info actual-fsm))
                   (tm (and (macher-agent--plist-p cur-info)
                            (plist-get cur-info :tracking-marker)))
                   (pm (and (macher-agent--plist-p cur-info)
                            (plist-get cur-info :position)))
                   (orig-tend (cond
                               ((and (markerp tm) (eq (marker-buffer tm) target-buf))
                                (marker-position tm))
                               ((and (markerp pm) (eq (marker-buffer pm) target-buf))
                                (marker-position pm))
                               (t nil))))
              (when (and orig-tend (macher-agent--plist-p cur-info))
                (setq cur-info (plist-put cur-info :macher-agent-orig-tools-end orig-tend))
                (macher-agent--set-fsm-info actual-fsm cur-info))))))
       ((stringp first-arg)
        (when (and wbuf (null (nth 2 cb-args)))
          (macher-agent--shadow-append-response-chunk wbuf actual-fsm first-arg))
        (apply orig-cb cb-args))
       (t
        (when wbuf
          (macher-agent--shadow-close-reasoning-fence wbuf actual-fsm))
        (apply orig-cb cb-args))))))

(defun macher-agent--fsm-hijack-transform (arg1 &optional arg2)
  "Mutate the FSM using ARG1 and optional ARG2 to inject hooks and media."
  (let* ((callback (cond ((functionp arg1) arg1)
                         ((functionp arg2) arg2)
                         (t nil)))
         (actual-fsm (cond ((and (recordp arg1) (not (functionp arg1))) arg1)
                           ((and arg2 (recordp arg2) (not (functionp arg2))) arg2)
                           (t (macher-agent-get-active-fsm))))
         (info (when actual-fsm (gptel-fsm-info actual-fsm)))
         (orig-cb (when info (plist-get info :callback)))
         (target-buf (when info (plist-get info :buffer))))

    (when (and target-buf (buffer-live-p target-buf))
      (with-current-buffer target-buf
        (setq-local macher-agent--active-fsm actual-fsm)
        (setq-local macher-agent--turn-submission-delivered nil))
      (let* ((ctx (macher-agent-resolve-context target-buf))
             (inf (gptel-fsm-info actual-fsm)))
        (setq inf (plist-put inf :origin-buffer target-buf))
        (when (eq (bound-and-true-p gptel-use-tools) 'force)
          (setq inf (plist-put inf :use-tools 'force)))
        (when (macher-agent-valid-context-p ctx)
          (setf (macher-agent-context-owner-buffer ctx) target-buf)
          (setq inf (plist-put inf :macher-agent-context ctx)))

        (let ((prompt (plist-get inf :prompt)))
          (when (and prompt (stringp prompt))
            (let ((raw-prompt (replace-regexp-in-string "^[#>*[:space:]]+" "" (string-trim prompt))))
              (setq inf (plist-put inf :prompt raw-prompt))
              (when ctx (setf (macher-agent-context-prompt ctx) raw-prompt)))))

        (macher-agent--set-fsm-info actual-fsm inf)
        (setf (gptel-fsm-info actual-fsm) inf)))

    (when (and actual-fsm orig-cb (not (plist-get (gptel-fsm-info actual-fsm) :macher-agent-cb-wrapped)))
      (let* ((inf (gptel-fsm-info actual-fsm))
             (wrapped (macher-agent--make-wrapped-fsm-callback actual-fsm target-buf orig-cb)))
        (setq inf (plist-put inf :callback wrapped))
        (setq inf (plist-put inf :macher-agent-cb-wrapped t))
        (macher-agent--set-fsm-info actual-fsm inf)
        (setf (gptel-fsm-info actual-fsm) inf)))

    (when actual-fsm
      (let* ((handlers (gptel-fsm-handlers actual-fsm))
             (all-states (delete-dups (append (mapcar #'car handlers) '(INIT WAIT TRET DONE ERRS ABRT))))
             (augmented-handlers
              (cl-loop for state in all-states
                       for funcs = (alist-get state handlers)
                       collect (cons state
                                     (cond
                                      ((eq state 'INIT)
                                       (if (memq #'macher-agent--apply-wire-prefix-to-fsm funcs)
                                           funcs
                                         (append funcs (list #'macher-agent--apply-wire-prefix-to-fsm))))
                                      ((eq state 'WAIT)
                                       (let ((f funcs))
                                         (unless (memq #'macher-agent--handle-wire-wait f)
                                           (setq f (cons #'macher-agent--handle-wire-wait f)))
                                         (unless (memq #'macher-agent--inject-media-fsm-logic f)
                                           (setq f (cons #'macher-agent--inject-media-fsm-logic f)))
                                         (unless (memq #'macher-agent--apply-wire-prefix-to-fsm f)
                                           (setq f (cons #'macher-agent--apply-wire-prefix-to-fsm f)))
                                         f))
                                      ((eq state 'TRET)
                                       (if (memq #'macher-agent--sanitize-fsm-tool-results funcs)
                                           funcs
                                         (cons #'macher-agent--sanitize-fsm-tool-results funcs)))
                                      ((eq state 'DONE)
                                       (let ((f funcs))
                                         (unless (memq #'macher-agent--capture-wire-done f)
                                           (setq f (cons #'macher-agent--capture-wire-done f)))
                                         (unless (memq #'macher-agent-gptel--trigger-flush f)
                                           (setq f (append f (list #'macher-agent-gptel--trigger-flush))))
                                         f))
                                      ((memq state '(ERRS ABRT))
                                       (let ((f funcs))
                                         (unless (memq #'macher-agent--rollback-wire-turn f)
                                           (setq f (cons #'macher-agent--rollback-wire-turn f)))
                                         (unless (memq #'macher-agent-gptel--trigger-flush f)
                                           (setq f (append f (list #'macher-agent-gptel--trigger-flush))))
                                         f))
                                      (t funcs))))))
        (macher-agent--set-fsm-handlers actual-fsm augmented-handlers)
        (setf (gptel-fsm-handlers actual-fsm) augmented-handlers))
      (macher-agent--install-tret-done-edge actual-fsm))

    (when callback (funcall callback))
    (when actual-fsm
      (macher-agent--apply-wire-prefix-to-fsm actual-fsm))))

(defun macher-agent--finalize-wire-transform (async-fn fsm)
  "Invoke ASYNC-FN and splice pending wire prefix onto FSM `:data'."
  (when (functionp async-fn)
    (funcall async-fn))
  (when fsm
    (macher-agent--apply-wire-prefix-to-fsm fsm)))

(defconst macher-agent--flv-override-vars
  '(gptel-system-prompt gptel-model gptel-temperature
                        gptel-max-tokens gptel--num-messages-to-send)
  "Buffer-local `gptel' variables whose file-local overrides take precedence.")

(defun macher-agent--flv-overrides ()
  "Return file-local `(VAR . VAL)' cells overriding preset defaults."
  (let ((flv (bound-and-true-p file-local-variables-alist)))
    (cl-loop for var in macher-agent--flv-override-vars
             for cell = (assq var flv)
             when cell collect cell)))

(defun macher-agent--clear-local-variables-gptel-props ()
  "Remove `gptel' properties from trailing HTML file-local variables block."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n")
    (when (and (> (point) (point-min))
               (save-excursion
                 (beginning-of-line)
                 (looking-at "[ \t]*<!--+[ \t]*End:[ \t]*--+>[ \t]*$")))
      (goto-char (line-beginning-position))
      (when (re-search-backward "^[ \t]*<!--+[ \t]*Local Variables:[ \t]*--+>[ \t]*$" nil t)
        (with-silent-modifications
          (remove-text-properties (match-beginning 0) (point-max)
                                  '(gptel nil front-sticky nil)))))))

(defun macher-agent--restore-saved-gptel-state ()
  "Restore saved gptel preset, tools, and bounds.
Preserves any file-local variable overrides."
  (when (and (buffer-file-name) (fboundp 'gptel--restore-state))
    (let* ((saved-overrides (macher-agent--flv-overrides))
           (preset-sym (bound-and-true-p gptel--preset))
           (preset-spec (when preset-sym
                          (macher-agent--lookup-skill-spec preset-sym nil (current-buffer)))))
      (when (and preset-spec (plist-get preset-spec :is-command))
        (setq-local gptel--preset nil))
      (ignore-errors (gptel--restore-state))
      (macher-agent--clear-local-variables-gptel-props)
      (dolist (cell saved-overrides)
        (set (make-local-variable (car cell)) (cdr cell))))))

(defun macher-agent--reconcile-restored-buffer (&optional force-restore)
  "Reconcile saved `gptel' session metadata in current buffer idempotently.
When FORCE-RESTORE is non-nil, run `macher-agent--restore-saved-gptel-state'
unconditionally; otherwise run it only when buffer state diverges."
  (when (buffer-file-name)
    (macher-agent--clear-local-variables-gptel-props))
  (let ((was-restored (bound-and-true-p macher-agent--is-restored-session))
        (needs-reconcile (or force-restore
                             (macher-agent--needs-preset-reconcile-p))))
    (when (or was-restored needs-reconcile)
      (setq-local macher-agent-presets nil)
      (setq-local macher-agent--is-restored-session nil)
      (when needs-reconcile
        (macher-agent--restore-saved-gptel-state))
      (macher-agent--clear-local-variables-gptel-props)
      (macher-agent--sync-preset-before-save)
      (when (fboundp 'gptel--update-header-line)
        (gptel--update-header-line)))))

(defun macher-agent--restore-local-state ()
  "Restore agent variables natively after Emacs parses file-local variables."
  (when (or (local-variable-p 'gptel--preset)
            (local-variable-p 'gptel--bounds)
            (local-variable-p 'gptel-model)
            (local-variable-p 'gptel-backend)
            (local-variable-p 'gptel--backend-name)
            (local-variable-p 'gptel--tool-names))
    (let ((current-root (macher-agent-root default-directory)))
      (when current-root
        (macher-agent--init-workspace-state current-root)))
    (macher-agent--reconcile-restored-buffer t)
    (setq-local macher-agent--is-restored-session t)))

(defun macher-agent--get-max-context-chars (&optional buf model-override)
  "Resolve max context characters based on MODEL-OVERRIDE or the model in BUF."
  (let* ((target-buf (or buf (current-buffer)))
         (model (or model-override
                    (when (buffer-live-p target-buf)
                      (buffer-local-value 'gptel-model target-buf))))
         (model-sym (if (stringp model) (intern model) model)))
    (or (alist-get model-sym macher-agent-max-context-chars)
        (alist-get nil macher-agent-max-context-chars)
        2000000)))

;;; Prompt Transformation and Inline Skill Extraction

(defun macher-agent--strip-wire-local-variables ()
  "Strip trailing HTML file-local variables block from current wire buffer."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n")
    (when (and (> (point) (point-min))
               (save-excursion
                 (beginning-of-line)
                 (looking-at "[ \t]*<!--+[ \t]*End:[ \t]*--+>[ \t]*$")))
      (goto-char (line-beginning-position))
      (when (re-search-backward "^[ \t]*<!--+[ \t]*Local Variables:[ \t]*--+>[ \t]*$" nil t)
        (let ((beg (match-beginning 0)))
          (unless (memq (car-safe (ensure-list (get-text-property beg 'gptel)))
                        '(response tool))
            (delete-region beg (point-max))))))))

(defun macher-agent--prompt-start-pos (&optional end-pos)
  "Return start position of the trailing user prompt before END-POS."
  (save-excursion
    (let ((limit (or end-pos (point-max))))
      (goto-char limit)
      (skip-chars-backward " \t\r\n")
      (while (and (> (point) (point-min))
                  (eq (get-text-property (1- (point)) 'gptel) 'ignore))
        (goto-char (or (previous-single-property-change (point) 'gptel)
                       (point-min)))
        (skip-chars-backward " \t\r\n"))
      (let ((prop-start
             (if (and (> (point) (point-min))
                      (get-text-property (1- (point)) 'gptel))
                 (point)
               (or (previous-single-property-change (point) 'gptel)
                   (point-min))))
            (wire-end (macher-agent--wire-last-committed-end (current-buffer) limit)))
        (if (and wire-end (> wire-end prop-start))
            wire-end
          prop-start)))))

(defun macher-agent--skip-prompt-prefix (pos)
  "Advance from POS past whitespace and prompt prefix (`### ', `*** ')."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t\r\n")
    (let ((pfx (when (fboundp 'gptel-prompt-prefix-string)
                 (ignore-errors (gptel-prompt-prefix-string)))))
      (cond
       ((and (stringp pfx)
             (not (string-empty-p pfx))
             (looking-at (regexp-quote pfx)))
        (goto-char (match-end 0)))
       ((looking-at "\\(?:#+\\|\\*+\\|>+\\)[ \t]+")
        (goto-char (match-end 0)))))
    (skip-chars-forward " \t")
    (point)))

(defun macher-agent--extract-inline-skills (prompt-start orig-buf)
  "Extract inline skill tags from PROMPT-START using ORIG-BUF context."
  (let ((matched-skills nil)
        (inline-preset-used nil)
        (known (with-current-buffer orig-buf (bound-and-true-p gptel--known-presets)))
        (first-non-ws (macher-agent--skip-prompt-prefix prompt-start))
        (stop nil))
    (save-excursion
      (goto-char prompt-start)
      (while (and (not stop) (< (point) (point-max)))
        (cond
         ((looking-at "^[ \t]*```")
          (forward-line 1)
          (unless (re-search-forward "^[ \t]*```[ \t]*$" nil 'move)
            (goto-char (point-max))))
         ((let ((case-fold-search t))
            (looking-at "^[ \t]*#\\+begin_\\(src\\|example\\|tool\\)\\b"))
          (let ((end-re (format "^[ \t]*#\\+end_%s\\b" (match-string 1)))
                (case-fold-search t))
            (forward-line 1)
            (unless (re-search-forward end-re nil 'move)
              (goto-char (point-max)))))
         ((re-search-forward "@\\([[:alnum:]_-]+\\)" (line-end-position) t)
          (when (and (not (eq (get-text-property (match-beginning 0) 'gptel) 'response))
                     (or (= (match-beginning 0) (point-min))
                         (memq (char-before (match-beginning 0)) '(32 ?\t ?\n ?\r 62))))
            (let* ((match-start (match-beginning 0))
                   (sym-name (match-string-no-properties 1))
                   (sym (intern sym-name))
                   (is-cmd (when (= match-start first-non-ws)
                             (with-current-buffer orig-buf
                               (let ((spec (macher-agent--lookup-skill-spec sym nil orig-buf)))
                                 (and spec (plist-get spec :is-command))))))
                   (is-valid
                    (or is-cmd
                        (with-current-buffer orig-buf
                          (or (assoc sym known)
                              (and (fboundp 'gptel-get-preset)
                                   (ignore-errors (gptel-get-preset sym)))
                              (when (bound-and-true-p macher-agent-global-skills-alist)
                                (assoc sym macher-agent-global-skills-alist))
                              (when-let* ((ctx (macher-agent-resolve-context orig-buf)))
                                (when (fboundp 'macher-agent-workspace-skills-alist)
                                  (ignore-errors (assoc sym (macher-agent-workspace-skills-alist ctx)))))
                              (when (fboundp 'gptel-get-tool)
                                (or (ignore-errors (gptel-get-tool sym-name))
                                    (ignore-errors (gptel-get-tool (replace-regexp-in-string "-" "_" sym-name)))
                                    (ignore-errors (gptel-get-tool (replace-regexp-in-string "_" "-" sym-name))))))))))
              (when is-valid
                (setq inline-preset-used t)
                (push sym matched-skills)
                (replace-match "")
                (when (looking-at "[ \t]+")
                  (replace-match ""))
                (when is-cmd
                  (setq stop t))))))
         (t
          (forward-line 1)))))
    (cons (nreverse matched-skills) inline-preset-used)))

(defun macher-agent--update-fsm-info-from-transmission-state (fsm state)
  "Update FSM info plist and encoded request data from transmission STATE."
  (when (and fsm state)
    (let* ((old-info (gptel-fsm-info fsm))
           (prev-backend (when (macher-agent--plist-p old-info)
                           (plist-get old-info :backend)))
           (new-info (copy-sequence old-info))
           (compiled (macher-agent-transmission-state-compiled-prompt state))
           (model (macher-agent-transmission-state-model state))
           (backend (macher-agent-transmission-state-backend state))
           (tools (macher-agent-transmission-state-tools state)))
      (setq new-info (plist-put new-info :system compiled))
      (when backend
        (setq new-info (plist-put new-info :backend backend)))
      (setq new-info (plist-put new-info :model model))
      (setq new-info (plist-put new-info :temperature (macher-agent-transmission-state-temperature state)))
      (setq new-info (plist-put new-info :max-tokens (macher-agent-transmission-state-max-tokens state)))
      (setq new-info (plist-put new-info :request-params (macher-agent-transmission-state-request-params state)))
      (setq new-info (plist-put new-info :disable-local-tools (macher-agent-transmission-state-disable-local-tools state)))
      (setq new-info (plist-put new-info :tools tools))
      (setq new-info (plist-put new-info :ptc-primitives
                                (macher-agent-transmission-state-ptc-primitives state)))
      (macher-agent--set-fsm-info fsm new-info)
      (macher-agent--sync-fsm-request-data
       fsm compiled tools model backend prev-backend))))

(defun macher-agent-transformer-deduplicate-tools (async-fn fsm)
  "Remove redundant tool usage blocks for FSM and invoke ASYNC-FN."
  (when (and macher-agent-max-duplicate-tools
             (> macher-agent-max-duplicate-tools 0))
    (let ((seen-tools (make-hash-table :test 'equal)))
      (save-excursion
        (goto-char (point-max))
        (while (re-search-backward "^[ \t]*\\(```\\|#\\+begin_src \\)tool\\(.*\\)$" nil t)
          (let* ((start (match-beginning 0))
                 (signature (save-match-data (string-trim (match-string 2))))
                 (is-markdown (string= (match-string 1) "```"))
                 (end-regex (if is-markdown "^[ \t]*```$" "^[ \t]*#\\+end_src$"))
                 (count (gethash signature seen-tools 0)))
            (when (or (get-text-property (match-beginning 1) 'gptel)
                      (get-text-property (match-beginning 1) 'gptel-tool)
                      (get-text-property (match-beginning 1) 'gptel-response)
                      (get-text-property (match-beginning 1) 'gptel-prompt)
                      (get-text-property start 'gptel)
                      (get-text-property start 'gptel-tool)
                      (get-text-property start 'gptel-response)
                      (get-text-property start 'gptel-prompt))
              (save-excursion
                (goto-char start)
                (when (re-search-forward end-regex nil t)
                  (let* ((content-start (save-excursion
                                          (goto-char start)
                                          (forward-line 1)
                                          (point)))
                         (content-end (match-beginning 0)))
                    (if (>= count macher-agent-max-duplicate-tools)
                        (when (< content-start content-end)
                          (delete-region content-start content-end)
                          (goto-char content-start)
                          (insert "{\"status\": \"omitted\", \"reason\": \"duplicate\"}\n"))
                      (puthash signature (1+ count) seen-tools)))))))))))
  (when-let* ((fn async-fn)
              ((functionp fn)))
    (funcall fn))
  (when fsm
    (macher-agent--apply-wire-prefix-to-fsm fsm)))

(defun macher-agent--transformer-sync-context (orig-buf context)
  "Synchronise CONTEXT struct to live ORIG-BUF."
  (cl-check-type orig-buf buffer)
  (cl-check-type context macher-agent-context)
  (when (buffer-live-p orig-buf)
    (macher-agent--inject-context-state context (buffer-local-value 'gptel-directives orig-buf))
    (run-hook-with-args 'macher-agent-context-sync-hook context))
  (let* ((active-fsm (macher-agent-get-active-fsm))
         (info-prompt (when active-fsm
                        (ignore-errors (plist-get (gptel-fsm-info active-fsm) :prompt)))))
    (when (and info-prompt (not (string-empty-p (string-trim info-prompt))))
      (setf (macher-agent-context-prompt context) info-prompt)))
  context)

(defun macher-agent--transformer-resolve-skills (buffer-presets inline-skills known &optional context target-buf)
  "Resolve transmission skills from BUFFER-PRESETS and INLINE-SKILLS.
Uses KNOWN presets, optional CONTEXT, and TARGET-BUF."
  (let ((combined (delete-dups (append buffer-presets inline-skills nil)))
        (ctx (or (and (macher-agent-valid-context-p context) context)
                 (macher-agent-resolve-context target-buf)))
        (acc nil)
        (exclusive-found nil))
    (dolist (p combined (nreverse acc))
      (let* ((spec (when (symbolp p)
                     (macher-agent--lookup-skill-spec p ctx target-buf known t)))
             (excl (plist-get spec :exclusive)))
        (if excl
            (progn
              (setq acc (list p))
              (setq exclusive-found t))
          (unless exclusive-found
            (push p acc)))))))

(defun macher-agent--transformer-detect-redirect (inline-preset-used inline-skills known ctx)
  "Detect whether INLINE-PRESET-USED triggers a command redirect.
Inspects INLINE-SKILLS against KNOWN presets and CTX."
  (when-let* ((_ inline-preset-used)
              (skill (car inline-skills)))
    (let* ((spec (when (symbolp skill)
                   (macher-agent--lookup-skill-spec skill ctx nil known t)))
           (is-command (or (and (listp skill) (plist-get skill :is-command))
                           (and spec (plist-get spec :is-command)))))
      (when is-command
        skill))))

(defun macher-agent-transmission-install ()
  "Register core transmission pipeline steps in strict execution order."
  (setq macher-agent-active-fsm-function #'macher-agent-get-active-fsm)
  (setq macher-agent-fsm-info-function #'gptel-fsm-info)
  (setq macher-agent-mid-turn-recompile-function #'macher-agent--recompile-and-sync-buffer-turn)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--hydrate-base-state 10)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--apply-skills-and-presets 20)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--extract-redirect 30)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-dynamic-context-tools 40)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--process-hidden-blocks 50)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--auto-retain-use-skill 53)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--auto-enable-task-tool 54)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--append-boot-directive 70)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-skill-directive 87)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--init-core-directives 95)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--compile-directives 100))

(defun macher-agent--merge-tools (tools1 tools2)
  "Merge TOOLS1 and TOOLS2, deduplicating by canonical tool name."
  (let ((seen (make-hash-table :test 'equal))
        (merged nil))
    (dolist (tool (append tools1 tools2))
      (when tool
        (let* ((name (macher-agent-canonical-tool-name tool))
               (key (if (stringp name)
                        (downcase (replace-regexp-in-string "-" "_" name))
                      name)))
          (unless (gethash key seen)
            (puthash key t seen)
            (push tool merged)))))
    (nreverse merged)))

(defun macher-agent--refresh-tool-struct (tool context)
  "Return refreshed `gptel-tool' struct for TOOL in CONTEXT when resolvable."
  (if (macher-tool-valid-p tool)
      (let* ((cname (macher-agent-canonical-tool-name tool))
             (resolved (when cname
                         (ignore-errors
                           (macher-agent-resolve-tool cname nil nil context)))))
        (if (macher-tool-valid-p resolved)
            resolved
          tool))
    tool))

(defun macher-agent--ensure-tool-in-list (tools tool-name &optional ctx target-buf fallback-var)
  "Return TOOLS with TOOL-NAME appended when not already present.
Resolves TOOL-NAME via CTX, TARGET-BUF `gptel-get-tool', or FALLBACK-VAR."
  (if (cl-some (lambda (tl)
                 (equal (macher-agent-canonical-tool-name tl) tool-name))
               tools)
      tools
    (let* ((resolved (ignore-errors (macher-agent-resolve-tool tool-name nil nil ctx)))
           (tool (or (and (macher-tool-valid-p resolved) resolved)
                     (and (fboundp 'gptel-get-tool)
                          (if (and (bufferp target-buf) (buffer-live-p target-buf))
                              (with-current-buffer target-buf
                                (ignore-errors (gptel-get-tool tool-name)))
                            (ignore-errors (gptel-get-tool tool-name))))
                     (and fallback-var (boundp fallback-var) (symbol-value fallback-var)))))
      (if (macher-tool-valid-p tool)
          (append tools (list tool))
        tools))))

(defun macher-agent-pipe--hydrate-base-state (state)
  "Extract pristine local variables from target buffer into STATE."
  (cl-check-type state macher-agent-transmission-state)
  (let ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                        (current-buffer))))
    (when (and target-buf (buffer-live-p target-buf))
      (with-current-buffer target-buf
        (let* ((active-context (or (macher-agent-transmission-state-context state)
                                   (macher-agent-resolve-context target-buf)))
               (flv (bound-and-true-p file-local-variables-alist))
               (buf-presets (bound-and-true-p macher-agent-presets))
               (active-preset (bound-and-true-p gptel--preset))
               (preset-spec (when (and (null buf-presets)
                                       (null (macher-agent-transmission-state-presets state))
                                       active-preset)
                              (let ((sp (macher-agent--lookup-skill-spec
                                         active-preset active-context target-buf)))
                                (when (and sp (not (plist-get sp :is-command)))
                                  sp))))
               (inherited (macher-agent-transmission-state-tools state))
               (local-p (local-variable-p 'gptel-tools target-buf))
               (buf-tools (when (boundp 'gptel-tools) gptel-tools))
               (was-toolless (buffer-local-value 'macher-agent--disable-local-tools target-buf))
               (tools-suppressed (and local-p (null buf-tools) (not was-toolless)))
               (merged-tools (unless tools-suppressed
                               (macher-agent--merge-tools inherited buf-tools)))
               (refreshed-tools
                (when merged-tools
                  (mapcar (lambda (tl)
                            (macher-agent--refresh-tool-struct tl active-context))
                          merged-tools))))
          (setf (macher-agent-transmission-state-backend state) (bound-and-true-p gptel-backend))
          (setf (macher-agent-transmission-state-model state) gptel-model)
          (setf (macher-agent-transmission-state-base-prompt state) gptel-system-prompt)
          (setf (macher-agent-transmission-state-compiled-prompt state) gptel-system-prompt)
          (setf (macher-agent-transmission-state-temperature state) (bound-and-true-p gptel-temperature))
          (setf (macher-agent-transmission-state-max-tokens state) (bound-and-true-p gptel-max-tokens))
          (setf (macher-agent-transmission-state-request-params state)
                (when (local-variable-p 'gptel--request-params target-buf)
                  (buffer-local-value 'gptel--request-params target-buf)))
          (setf (macher-agent-transmission-state-disable-local-tools state)
                (buffer-local-value 'macher-agent--disable-local-tools target-buf))
          (setf (macher-agent-transmission-state-tools state) refreshed-tools)
          (setf (macher-agent-transmission-state-ptc-primitives state)
                (bound-and-true-p macher-agent--active-ptc-primitives))
          (setf (macher-agent-transmission-state-known-presets state)
                (when (local-variable-p 'gptel--known-presets target-buf)
                  (buffer-local-value 'gptel--known-presets target-buf)))
          (when preset-spec
            (when (and (macher-agent-transmission-state-disable-local-tools state)
                       (not (plist-get preset-spec :disable-local-tools)))
              (setf (macher-agent-transmission-state-disable-local-tools state) nil))
            (when (and (not (assq 'gptel--request-params flv))
                       (not (plist-member preset-spec :request-params))
                       (local-variable-p 'macher-agent--preset-request-params target-buf)
                       (buffer-local-value 'macher-agent--preset-request-params target-buf)
                       (equal (macher-agent-transmission-state-request-params state)
                              (buffer-local-value 'macher-agent--preset-request-params target-buf)))
              (setf (macher-agent-transmission-state-request-params state) nil))
            (let* ((has-flv-tools
                    (or (assq 'gptel--tool-names flv)
                        (assq 'gptel-tools flv)
                        (and (derived-mode-p 'org-mode)
                             (fboundp 'org-entry-get)
                             (org-entry-get (point-min) "GPTEL_TOOLS" 'selective))))
                   (has-flv-sys (assq 'gptel-system-prompt flv))
                   (has-parents (plist-get preset-spec :parents))
                   (raw-sys (unless has-flv-sys
                              (or (plist-get preset-spec :system)
                                  (plist-get preset-spec :system-message))))
                   (direct-sys
                    (when (and raw-sys
                               (not has-parents)
                               (not (and (consp raw-sys) (keywordp (car raw-sys)))))
                      (cond
                       ((stringp raw-sys) raw-sys)
                       ((and (symbolp raw-sys) (not (functionp raw-sys)))
                        (or (alist-get raw-sys (bound-and-true-p gptel-directives))
                            raw-sys))
                       ((consp raw-sys) raw-sys)
                       (t nil))))
                   (filtered-spec (copy-sequence preset-spec)))
              (setq filtered-spec (plist-put filtered-spec :name (symbol-name active-preset)))
              (when (assq 'gptel-model flv)
                (cl-remf filtered-spec :model))
              (when (or (assq 'gptel-backend flv) (assq 'gptel--backend-name flv))
                (cl-remf filtered-spec :backend))
              (when (assq 'gptel-temperature flv)
                (cl-remf filtered-spec :temperature))
              (when (assq 'gptel-max-tokens flv)
                (cl-remf filtered-spec :max-tokens))
              (when (assq 'gptel--request-params flv)
                (cl-remf filtered-spec :request-params))
              (when (or has-flv-sys direct-sys)
                (cl-remf filtered-spec :system)
                (cl-remf filtered-spec :system-message))
              (when (or tools-suppressed has-flv-tools)
                (cl-remf filtered-spec :tools)
                (cl-remf filtered-spec :allowed-tools))
              (let ((composed
                     (macher-agent-compose-payload
                      (list :known-presets (macher-agent-transmission-state-known-presets state)
                            :backend (macher-agent-transmission-state-backend state)
                            :model (macher-agent-transmission-state-model state)
                            :temperature (macher-agent-transmission-state-temperature state)
                            :max-tokens (macher-agent-transmission-state-max-tokens state)
                            :request-params (macher-agent-transmission-state-request-params state)
                            :disable-local-tools (macher-agent-transmission-state-disable-local-tools state)
                            :system (or direct-sys
                                        (macher-agent-transmission-state-base-prompt state))
                            :tools refreshed-tools
                            :ptc-primitives (macher-agent-transmission-state-ptc-primitives state))
                      (list filtered-spec))))
                (setf (macher-agent-transmission-state-model state) (plist-get composed :model))
                (setf (macher-agent-transmission-state-backend state) (plist-get composed :backend))
                (setf (macher-agent-transmission-state-temperature state) (plist-get composed :temperature))
                (setf (macher-agent-transmission-state-max-tokens state) (plist-get composed :max-tokens))
                (unless (assq 'gptel--request-params flv)
                  (setf (macher-agent-transmission-state-request-params state)
                        (plist-get composed :request-params)))
                (setf (macher-agent-transmission-state-disable-local-tools state)
                      (plist-get composed :disable-local-tools))
                (unless has-flv-sys
                  (let ((final-sys (or direct-sys (plist-get composed :system))))
                    (setf (macher-agent-transmission-state-base-prompt state) final-sys)
                    (setf (macher-agent-transmission-state-compiled-prompt state) final-sys)))
                (if (macher-agent-transmission-state-disable-local-tools state)
                    (progn
                      (setf (macher-agent-transmission-state-tools state) nil)
                      (setf (macher-agent-transmission-state-ptc-primitives state) nil))
                  (when (and (not tools-suppressed) (not has-flv-tools))
                    (setf (macher-agent-transmission-state-tools state)
                          (mapcar (lambda (tl)
                                    (macher-agent--refresh-tool-struct tl active-context))
                                  (plist-get composed :tools))))
                  (setf (macher-agent-transmission-state-ptc-primitives state)
                        (plist-get composed :ptc-primitives))))))
          (when (macher-agent-transmission-state-disable-local-tools state)
            (setf (macher-agent-transmission-state-tools state) nil)
            (setf (macher-agent-transmission-state-ptc-primitives state) nil))))))
  state)

(defun macher-agent-pipe--apply-skills-and-presets (state)
  "Merge active presets and transmission skills into STATE."
  (cl-check-type state macher-agent-transmission-state)
  (let ((combined-modules (macher-agent-transmission-state-skills state))
        (target-buf (macher-agent-transmission-state-target-buffer state)))
    (when combined-modules
      (let* ((known (macher-agent-transmission-state-known-presets state))
             (payload-plist (list :known-presets known
                                  :backend (macher-agent-transmission-state-backend state)
                                  :model (macher-agent-transmission-state-model state)
                                  :system (macher-agent-transmission-state-base-prompt state)
                                  :tools (macher-agent-transmission-state-tools state)
                                  :ptc-primitives (macher-agent-transmission-state-ptc-primitives state)
                                  :temperature (macher-agent-transmission-state-temperature state)
                                  :max-tokens (macher-agent-transmission-state-max-tokens state)
                                  :request-params (macher-agent-transmission-state-request-params state)
                                  :disable-local-tools (macher-agent-transmission-state-disable-local-tools state)))
             (composed (if (and target-buf (buffer-live-p target-buf))
                           (with-current-buffer target-buf
                             (macher-agent-compose-payload payload-plist combined-modules))
                         (macher-agent-compose-payload payload-plist combined-modules))))
        (when (plist-member composed :system)
          (setf (macher-agent-transmission-state-base-prompt state) (plist-get composed :system)))
        (when (plist-get composed :backend)
          (setf (macher-agent-transmission-state-backend state) (plist-get composed :backend)))
        (when (plist-get composed :model)
          (setf (macher-agent-transmission-state-model state) (plist-get composed :model)))
        (when (plist-get composed :temperature)
          (setf (macher-agent-transmission-state-temperature state) (plist-get composed :temperature)))
        (when (plist-get composed :max-tokens)
          (setf (macher-agent-transmission-state-max-tokens state) (plist-get composed :max-tokens)))
        (when (plist-member composed :request-params)
          (setf (macher-agent-transmission-state-request-params state) (plist-get composed :request-params)))
        (when (plist-member composed :disable-local-tools)
          (setf (macher-agent-transmission-state-disable-local-tools state)
                (plist-get composed :disable-local-tools)))
        (if (macher-agent-transmission-state-disable-local-tools state)
            (progn
              (setf (macher-agent-transmission-state-tools state) nil)
              (setf (macher-agent-transmission-state-ptc-primitives state) nil))
          (when (plist-member composed :tools)
            (setf (macher-agent-transmission-state-tools state) (plist-get composed :tools)))
          (when (plist-member composed :ptc-primitives)
            (setf (macher-agent-transmission-state-ptc-primitives state)
                  (plist-get composed :ptc-primitives)))))))
  state)

(defun macher-agent-pipe--inject-dynamic-context-tools (state)
  "Inject dynamic context tools into STATE when buffer exceeds `max-chars'."
  (cl-check-type state macher-agent-transmission-state)
  (unless (macher-agent-transmission-state-disable-local-tools state)
    (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                           (current-buffer)))
           (max-chars (macher-agent--get-max-context-chars
                       target-buf (macher-agent-transmission-state-model state)))
           (buf-size (if (and target-buf (buffer-live-p target-buf))
                         (max (with-current-buffer target-buf (buffer-size))
                              (macher-agent--wire-total-chars target-buf))
                       0)))
      (when (> buf-size max-chars)
        (let ((ctx (or (macher-agent-transmission-state-context state)
                       (macher-agent-resolve-context target-buf))))
          (setf (macher-agent-transmission-state-tools state)
                (macher-agent--ensure-tool-in-list
                 (macher-agent-transmission-state-tools state)
                 "search_conversation_history" ctx target-buf))))))
  state)

(defun macher-agent-pipe--process-hidden-blocks (state)
  "Strip UI-specific or hidden metadata from STATE before transmission."
  (cl-check-type state macher-agent-transmission-state)
  state)

(defun macher-agent-pipe--auto-retain-use-skill (state)
  "Retain `use_skill' in STATE when target buffer has cached presets."
  (cl-check-type state macher-agent-transmission-state)
  (unless (macher-agent-transmission-state-disable-local-tools state)
    (let ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                          (current-buffer))))
      (when (macher-agent--has-cached-skill-state-p target-buf)
        (let ((ctx (or (macher-agent-transmission-state-context state)
                       (macher-agent-resolve-context target-buf))))
          (setf (macher-agent-transmission-state-tools state)
                (macher-agent--ensure-tool-in-list
                 (macher-agent-transmission-state-tools state)
                 "use_skill" ctx target-buf 'macher-agent-use-skill-tool))))))
  state)

(defun macher-agent-pipe--auto-enable-task-tool (state)
  "Auto-enable `submit_task_result' in STATE when a task or route is active."
  (cl-check-type state macher-agent-transmission-state)
  (unless (macher-agent-transmission-state-disable-local-tools state)
    (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                           (current-buffer)))
           (has-task (when (and target-buf (buffer-live-p target-buf))
                       (or (buffer-local-value 'macher-agent--routing-stack target-buf)
                           (buffer-local-value 'macher-agent--current-task-id target-buf)))))
      (when has-task
        (let ((ctx (or (macher-agent-transmission-state-context state)
                       (macher-agent-resolve-context target-buf))))
          (setf (macher-agent-transmission-state-tools state)
                (macher-agent--ensure-tool-in-list
                 (macher-agent-transmission-state-tools state)
                 "submit_task_result" ctx target-buf 'macher-agent-submit-task-result-tool))))))
  state)

(defun macher-agent-pipe--init-core-directives (state)
  "Mandate task submission in STATE if `submit_task_result' is enabled."
  (cl-check-type state macher-agent-transmission-state)
  (when (cl-some (lambda (tool)
                   (equal (macher-agent-canonical-tool-name tool) "submit_task_result"))
                 (macher-agent-transmission-state-tools state))
    (push "CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool to submit your final answer when you are completely finished. Do NOT output your final answer as standard text. IMMEDIATELY STOP after."
          (macher-agent-transmission-state-directives state)))
  state)

(defun macher-agent-pipe--append-boot-directive (state)
  "Append boot directive to STATE struct."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (boot-dir (when (and target-buf (buffer-live-p target-buf))
                     (with-current-buffer target-buf
                       (bound-and-true-p macher-agent--boot-directive))))
         (forced (when (and target-buf (buffer-live-p target-buf))
                   (buffer-local-value 'macher-agent--force-boot-directive target-buf))))
    (when (and boot-dir (stringp boot-dir) (not (string-empty-p boot-dir))
               target-buf
               (or forced
                   (and (not (macher-agent--wire-has-spans-p target-buf))
                        (not (with-current-buffer target-buf
                               (text-property-any (point-min) (point-max) 'gptel 'response))))))
      (push boot-dir (macher-agent-transmission-state-directives state))
      (when (and forced
                 (macher-agent-transmission-state-consume-boot-directive state)
                 (buffer-live-p target-buf))
        (with-current-buffer target-buf
          (setq-local macher-agent--force-boot-directive nil)))))
  state)

(defun macher-agent-pipe--inject-skill-directive (state)
  "Inject available non-command skills into STATE when `use_skill' is active."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((tools (macher-agent-transmission-state-tools state))
         (has-use-skill (cl-some (lambda (tool)
                                   (equal (macher-agent-canonical-tool-name tool) "use_skill"))
                                 tools)))
    (when has-use-skill
      (let* ((target-buf (macher-agent-transmission-state-target-buffer state))
             (buf (if (and target-buf (buffer-live-p target-buf))
                      target-buf
                    (current-buffer)))
             (ctx (or (macher-agent-transmission-state-context state)
                      (macher-agent-resolve-context target-buf)))
             (buffer-presets
              (or (and (local-variable-p 'gptel--known-presets buf)
                       (buffer-local-value 'gptel--known-presets buf))
                  (and (macher-agent-valid-context-p ctx)
                       (macher-agent-workspace-skills-alist ctx))
                  (and (local-variable-p 'macher-agent-presets buf)
                       (buffer-local-value 'macher-agent-presets buf))
                  (macher-agent-transmission-state-known-presets state)
                  (macher-agent-transmission-state-presets state)))
             (presets-list (if (and buffer-presets (atom buffer-presets))
                               (list buffer-presets)
                             buffer-presets))
             (seen (make-hash-table :test 'equal))
             (skill-lines nil))
        (dolist (entry presets-list)
          (let* ((sym (if (consp entry) (car entry) entry))
                 (base-spec (when (and (consp entry) (listp (cdr entry))) (cdr entry)))
                 (lookup-spec (when (or (symbolp sym) (stringp sym))
                                (macher-agent--lookup-skill-spec sym ctx target-buf)))
                 (spec (if (symbolp sym)
                           (when (or base-spec lookup-spec)
                             (macher-agent--merge-preset-meta sym base-spec lookup-spec))
                         (or base-spec lookup-spec)))
                 (is-cmd (and (listp spec) (plist-get spec :is-command)))
                 (toolless (and (listp spec) (plist-get spec :disable-local-tools)))
                 (name (cond ((symbolp sym) (symbol-name sym))
                             ((stringp sym) sym)
                             (t (format "%s" sym)))))
            (unless (or is-cmd toolless (gethash name seen))
              (puthash name t seen)
              (let ((desc (cond
                           ((and (listp spec) (plist-get spec :description))
                            (plist-get spec :description))
                           ((and (consp entry) (stringp (cdr entry)))
                            (cdr entry))
                           (t nil))))
                (push (if (and desc (stringp desc) (not (string-empty-p (string-trim desc))))
                          (format "- %s: %s" name (string-trim desc))
                        (format "- %s" name))
                      skill-lines)))))
        (let* ((skills-text (if skill-lines
                                (string-join (nreverse skill-lines) "\n")
                              ""))
               (directive (if (and skills-text (not (string-empty-p skills-text)))
                              (format "<available_skills>\n%s\n</available_skills>" skills-text)
                            "<available_skills>\n</available_skills>")))
          (push directive (macher-agent-transmission-state-directives state))))))
  state)

(defun macher-agent-pipe--compile-directives (state)
  "Compile collected directives in STATE and append them to base prompt."
  (cl-check-type state macher-agent-transmission-state)
  (let ((dirs (nreverse (macher-agent-transmission-state-directives state)))
        (sys (macher-agent-transmission-state-base-prompt state)))
    (if dirs
        (let ((compiled-directives (string-join dirs "\n\n")))
          (setf (macher-agent-transmission-state-compiled-prompt state)
                (if (and sys (not (string-empty-p sys)))
                    (concat sys "\n\n" compiled-directives)
                  compiled-directives)))
      (setf (macher-agent-transmission-state-compiled-prompt state) sys)))
  state)

(defun macher-agent--interpolate-command-body (body user-args)
  "Interpolate USER-ARGS into command BODY in a single pass."
  (let* ((args-list (cond
                     ((listp user-args) user-args)
                     ((stringp user-args)
                      (let ((trimmed (string-trim user-args)))
                        (if (string-empty-p trimmed)
                            nil
                          (or (ignore-errors (split-string-and-unquote trimmed))
                              (split-string trimmed)))))
                     (t nil)))
         (full-args (if (stringp user-args)
                        (string-trim user-args)
                      (mapconcat #'identity (or args-list nil) " "))))
    (replace-regexp-in-string
     "\\$\\(arguments\\|args\\|[1-9][0-9]*\\)\\b"
     (lambda (match)
       (let ((token (substring match 1)))
         (cond
          ((or (string= token "arguments") (string= token "args"))
           full-args)
          (t
           (let ((idx (1- (string-to-number token))))
             (if (and (>= idx 0) (< idx (length args-list)))
                 (nth idx args-list)
               match))))))
     body t t)))

(defun macher-agent--compile-transmission-payload (orig-buf presets skills redirected-skill &optional context consume-boot)
  "Compile transmission payload for live ORIG-BUF.
Applies PRESETS, SKILLS, and REDIRECTED-SKILL with optional CONTEXT and
CONSUME-BOOT mode."
  (cl-check-type orig-buf buffer)
  (let* ((active-context (or context (macher-agent-resolve-context orig-buf)))
         (known (when (buffer-live-p orig-buf)
                  (buffer-local-value 'gptel--known-presets orig-buf)))
         (clean-skills (seq-remove (lambda (s)
                                     (let ((spec (if (listp s) s
                                                   (macher-agent--lookup-skill-spec
                                                    s active-context orig-buf known t))))
                                       (plist-get spec :is-command)))
                                   skills))
         (initial-state (make-macher-agent-transmission-state
                         :target-buffer orig-buf
                         :presets presets
                         :skills clean-skills
                         :redirected-skill redirected-skill
                         :context active-context
                         :consume-boot-directive (if (null consume-boot)
                                                     t
                                                   (not (eq consume-boot :preserve)))))
         (all-steps (macher-agent-get-pipeline-steps 'transmission)))
    (seq-reduce (lambda (st pipe-fn)
                  (cl-check-type st macher-agent-transmission-state)
                  (if (functionp pipe-fn)
                      (funcall pipe-fn st)
                    st))
                all-steps
                initial-state)))

(defun macher-agent--assign-missing-buffer-tool-ids ()
  "Assign deterministic IDs to `(tool . nil)' regions in current buffer.
Returns `(COUNTS . USED)' hash tables tracking per-tool call counts and IDs."
  (let ((counts (make-hash-table :test 'equal))
        (used (make-hash-table :test 'equal))
        (inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((prop (get-char-property (point) 'gptel))
              (next (next-single-property-change (point) 'gptel nil (point-max))))
          (when (and (consp prop) (eq (car prop) 'tool))
            (let* ((form (save-excursion (ignore-errors (read (current-buffer)))))
                   (tname (or (and (listp form) (plist-get form :name)) "tool"))
                   (n (1+ (gethash tname counts 0)))
                   (existing-id (cdr prop))
                   (chosen-id
                    (or existing-id
                        (let ((cand (if (= n 1)
                                        (format "call_%s" tname)
                                      (format "call_%s_%d" tname n))))
                          (while (gethash cand used)
                            (setq n (1+ n)
                                  cand (format "call_%s_%d" tname n)))
                          cand))))
              (puthash tname n counts)
              (puthash (format "%s" chosen-id) t used)
              (unless existing-id
                (put-text-property (point) next 'gptel (cons 'tool chosen-id)))))
          (goto-char next))))
    (cons counts used)))

(defun macher-agent--ensure-tool-use-ids (tool-use &optional counts used)
  "Assign deterministic `:id' values to entries in TOOL-USE lacking one.
Uses optional COUNTS and USED tables from prior buffer tool blocks."
  (when (and (listp tool-use) tool-use)
    (let ((counts (or counts (make-hash-table :test 'equal)))
          (used (or used (make-hash-table :test 'equal))))
      (dolist (tc tool-use)
        (when-let* ((id (and (macher-agent--plist-p tc) (plist-get tc :id))))
          (puthash (format "%s" id) t used)))
      (dolist (tc tool-use)
        (when (and (macher-agent--plist-p tc) (not (plist-get tc :id)))
          (let* ((tname (or (plist-get tc :name) "tool"))
                 (n (1+ (gethash tname counts 0)))
                 (cand (if (= n 1)
                           (format "call_%s" tname)
                         (format "call_%s_%d" tname n))))
            (while (gethash cand used)
              (setq n (1+ n)
                    cand (format "call_%s_%d" tname n)))
            (puthash tname n counts)
            (puthash cand t used)
            (plist-put tc :id cand)))))))

(defun macher-agent--merge-parallel-open-prompts (items)
  "Merge consecutive ITEMS sharing a `:role' by concatenating vector fields."
  (let ((acc nil))
    (dolist (item items (nreverse acc))
      (let ((prev (car acc))
            (role (and (listp item) (plist-get item :role))))
        (if (and role (listp prev) (equal role (plist-get prev :role)))
            (let ((merged (copy-sequence prev))
                  (tail item))
              (while (consp tail)
                (let ((k (pop tail))
                      (v (pop tail)))
                  (when (and (vectorp v) (vectorp (plist-get merged k)))
                    (setq merged (plist-put merged k (vconcat (plist-get merged k) v))))))
              (setcar acc merged))
          (push item acc))))))

(defun macher-agent--existing-prompts-from-data (data)
  "Extract non-system prompt list from encoded request DATA."
  (let ((vec (cond
              ((plist-member data :contents) (plist-get data :contents))
              ((plist-member data :input) (plist-get data :input))
              ((plist-member data :messages) (plist-get data :messages)))))
    (when (or (vectorp vec) (listp vec))
      (cl-remove-if
       (lambda (msg)
         (and (listp msg)
              (member (plist-get msg :role) '("system" "developer"))))
       (append vec nil)))))

(defun macher-agent--parse-buffer-slice (buf beg end backend &optional strip-skills orig-ref-buf id-tables-cell)
  "Parse BUF between BEG and END into BACKEND prompts.
When STRIP-SKILLS is non-nil, strip inline `@skill' tags relative to
ORIG-REF-BUF.  When ID-TABLES-CELL is a cons cell, store `(COUNTS . USED)'
in its car."
  (when (and (bufferp buf) (buffer-live-p buf)
             backend (fboundp 'gptel--create-prompt-buffer))
    (with-current-buffer buf
      (let ((b (max (point-min) (min (point-max) beg)))
            (e (max (point-min) (min (point-max) end))))
        (when (and (< b e)
                   (not (string-blank-p (buffer-substring-no-properties b e))))
          (save-excursion
            (save-restriction
              (narrow-to-region b e)
              (goto-char (point-max))
              (let* ((mark-active nil)
                     (copy-buf (gptel--create-prompt-buffer (point))))
                (unwind-protect
                    (with-current-buffer copy-buf
                      (macher-agent--strip-wire-local-variables)
                      (when strip-skills
                        (macher-agent--extract-inline-skills
                         (point-min) (or orig-ref-buf buf)))
                      (let ((tables (macher-agent--assign-missing-buffer-tool-ids)))
                        (when (consp id-tables-cell)
                          (setcar id-tables-cell tables)))
                      (setq-local gptel-track-response t)
                      (setq-local gptel-backend backend)
                      (goto-char (point-max))
                      (unless (string-blank-p
                               (buffer-substring-no-properties (point-min) (point-max)))
                        (gptel--parse-buffer backend nil)))
                  (when (buffer-live-p copy-buf)
                    (kill-buffer copy-buf)))))))))))

(defun macher-agent--collect-wire-spans (wbuf)
  "Scan shadow wire buffer WBUF and return ordered list of span plists."
  (when (and (bufferp wbuf) (buffer-live-p wbuf))
    (with-current-buffer wbuf
      (let ((pos (point-min))
            (spans nil))
        (while (< pos (point-max))
          (let ((sid (get-text-property pos 'macher-agent-span-id))
                (next (next-single-property-change
                       pos 'macher-agent-span-id wbuf (point-max))))
            (when sid
              (push (list :span-id sid
                          :turn-id (get-text-property pos 'macher-agent-turn-id)
                          :kind (get-text-property pos 'macher-agent-span-kind)
                          :entries (get-text-property pos 'macher-agent-wire-entries)
                          :backend-type (get-text-property pos 'macher-agent-wire-backend-type)
                          :chars (or (get-text-property pos 'macher-agent-wire-chars) 0)
                          :orig-beg (get-text-property pos 'macher-agent-orig-beg)
                          :orig-end (get-text-property pos 'macher-agent-orig-end)
                          :prompt-beg (get-text-property pos 'macher-agent-prompt-beg)
                          :prompt-end (get-text-property pos 'macher-agent-prompt-end)
                          :nonempty (get-text-property pos 'macher-agent-orig-nonempty)
                          :norm-hash (get-text-property pos 'macher-agent-norm-hash)
                          :sbeg pos
                          :send next)
                    spans))
            (setq pos next)))
        (nreverse spans)))))

(defun macher-agent--find-shadow-span-bounds (wbuf span-id)
  "Return live `(SBEG . SEND)' bounds for SPAN-ID in shadow buffer WBUF."
  (when (and span-id (bufferp wbuf) (buffer-live-p wbuf))
    (with-current-buffer wbuf
      (let ((pos (point-min))
            (found nil))
        (while (and (not found) (< pos (point-max)))
          (let ((sid (get-text-property pos 'macher-agent-span-id))
                (next (next-single-property-change
                       pos 'macher-agent-span-id wbuf (point-max))))
            (when (eql sid span-id)
              (setq found (cons pos next)))
            (setq pos next)))
        found))))

(defun macher-agent--prune-shadow-spans (wbuf spans &optional orig-buf)
  "Delete SPANS from shadow wire buffer WBUF and detach unreferenced markers.
When ORIG-BUF (or `macher-agent--wire-origin-buffer') is live and any
span is removed, run `macher-agent-wire-invalidated-hook' with `pruned'."
  (when (and spans (bufferp wbuf) (buffer-live-p wbuf))
    (let ((target-orig (or orig-buf
                           (buffer-local-value 'macher-agent--wire-origin-buffer wbuf)))
          (removed nil))
      (with-current-buffer wbuf
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (candidate-markers nil)
              (still-used (make-hash-table :test 'eq))
              (ranges nil))
          (dolist (sp spans)
            (let* ((live (when-let* ((sid (plist-get sp :span-id)))
                           (macher-agent--find-shadow-span-bounds wbuf sid)))
                   (sb (max (point-min) (min (point-max)
                                             (or (car-safe live) (plist-get sp :sbeg)))))
                   (se (max (point-min) (min (point-max)
                                             (or (cdr-safe live) (plist-get sp :send))))))
              (when (< sb se)
                (push (cons sb se) ranges))))
          (setq ranges (sort ranges (lambda (a b) (> (car a) (car b)))))
          (dolist (rng ranges)
            (let ((sb (max (point-min) (min (point-max) (car rng))))
                  (se (max (point-min) (min (point-max) (cdr rng)))))
              (when (< sb se)
                (setq removed t)
                (let ((pos sb))
                  (while (< pos se)
                    (dolist (prop '(macher-agent-orig-beg
                                    macher-agent-orig-end
                                    macher-agent-prompt-beg
                                    macher-agent-prompt-end))
                      (let ((m (get-text-property pos prop)))
                        (when (markerp m)
                          (push m candidate-markers))))
                    (setq pos (next-property-change pos wbuf se))))
                (delete-region sb se))))
          (when candidate-markers
            (let ((pos (point-min)))
              (while (< pos (point-max))
                (dolist (prop '(macher-agent-orig-beg
                                macher-agent-orig-end
                                macher-agent-prompt-beg
                                macher-agent-prompt-end))
                  (let ((m (get-text-property pos prop)))
                    (when (markerp m)
                      (puthash m t still-used))))
                (setq pos (next-property-change pos wbuf (point-max)))))
            (dolist (m candidate-markers)
              (unless (gethash m still-used)
                (set-marker m nil))))))
      (when (and removed (bufferp target-orig) (buffer-live-p target-orig))
        (unless (macher-agent--wire-has-spans-p target-orig)
          (with-current-buffer target-orig
            (setq-local macher-agent--wire-horizon-turn-id nil)
            (setq-local macher-agent--wire-horizon-lines-deleted nil)
            (setq-local macher-agent--wire-dedup-turn-id nil)))
        (run-hook-with-args 'macher-agent-wire-invalidated-hook target-orig 'pruned)))))

(defun macher-agent--insert-slice-with-shadow-props (wbuf orig-buf beg end kind)
  "Insert ORIG-BUF `[BEG, END)' text into WBUF at point for span KIND.
Preserves `gptel' response/tool semantics and ensures trailing newline
separation.  Returns `(INS-BEG . INS-END)' in WBUF."
  (let* ((raw-str (with-current-buffer orig-buf
                    (let ((b (max (point-min) (min (point-max) beg)))
                          (e (max (point-min) (min (point-max) end))))
                      (if (< b e) (buffer-substring b e) ""))))
         (trimmed (string-trim-right raw-str))
         (is-resp-kind (memq kind '(model-final tool-step)))
         (ins-beg nil)
         (ins-end nil))
    (with-current-buffer wbuf
      (setq ins-beg (point))
      (unless (string-empty-p trimmed)
        (insert trimmed)
        (let ((pos ins-beg)
              (lim (point)))
          (while (< pos lim)
            (let ((gp (get-text-property pos 'gptel))
                  (next (next-single-property-change pos 'gptel wbuf lim)))
              (cond
               ((eq kind 'user-prompt)
                (set-text-properties pos next nil))
               (is-resp-kind
                (if (or (eq gp 'ignore) (and (consp gp) (eq (car gp) 'tool)))
                    (set-text-properties pos next (list 'gptel gp))
                  (set-text-properties pos next '(gptel response))))
               (t
                (if (or (eq gp 'response)
                        (eq gp 'ignore)
                        (and (consp gp) (eq (car gp) 'tool)))
                    (set-text-properties pos next (list 'gptel gp))
                  (set-text-properties pos next nil))))
              (setq pos next)))))
      (let ((sep-start (point)))
        (insert "\n\n")
        (if is-resp-kind
            (set-text-properties sep-start (point) '(gptel ignore))
          (set-text-properties sep-start (point) nil)))
      (setq ins-end (point)))
    (cons ins-beg ins-end)))

(defun macher-agent--update-shadow-span (wbuf span new-entries new-hash backend-type &optional orig-buf)
  "Update SPAN in WBUF with edited ORIG-BUF text and NEW-ENTRIES.
Stores NEW-HASH and BACKEND-TYPE on the updated span."
  (when (and (bufferp wbuf) (buffer-live-p wbuf))
    (let* ((target-orig (or orig-buf
                            (buffer-local-value 'macher-agent--wire-origin-buffer wbuf)))
           (live (when-let* ((sid (plist-get span :span-id)))
                   (macher-agent--find-shadow-span-bounds wbuf sid)))
           (copied (macher-agent--deep-copy-wire-data new-entries))
           (updated nil))
      (with-current-buffer wbuf
        (let* ((inhibit-read-only t)
               (inhibit-modification-hooks t)
               (sb (max (point-min) (min (point-max)
                                         (or (car-safe live) (plist-get span :sbeg)))))
               (se (max (point-min) (min (point-max)
                                         (or (cdr-safe live) (plist-get span :send))))))
          (when (< sb se)
            (let* ((sid (or (plist-get span :span-id)
                            (get-text-property sb 'macher-agent-span-id)))
                   (tid (or (plist-get span :turn-id)
                            (get-text-property sb 'macher-agent-turn-id)))
                   (kind (or (plist-get span :kind)
                             (get-text-property sb 'macher-agent-span-kind)))
                   (ob-m (or (plist-get span :orig-beg)
                             (get-text-property sb 'macher-agent-orig-beg)))
                   (oe-m (or (plist-get span :orig-end)
                             (get-text-property sb 'macher-agent-orig-end)))
                   (pb-m (or (plist-get span :prompt-beg)
                             (get-text-property sb 'macher-agent-prompt-beg)))
                   (pe-m (or (plist-get span :prompt-end)
                             (get-text-property sb 'macher-agent-prompt-end)))
                   (nonempty (not (eql new-hash (sxhash-equal ""))))
                   (span-props
                    (append
                     (list 'macher-agent-span-id sid
                           'macher-agent-turn-id tid
                           'macher-agent-span-kind kind
                           'macher-agent-wire-entries copied
                           'macher-agent-norm-hash new-hash
                           'macher-agent-wire-backend-type backend-type
                           'macher-agent-wire-chars (macher-agent--wire-estimate-chars copied)
                           'macher-agent-orig-beg ob-m
                           'macher-agent-orig-end oe-m
                           'macher-agent-orig-nonempty nonempty)
                     (when pb-m (list 'macher-agent-prompt-beg pb-m))
                     (when pe-m (list 'macher-agent-prompt-end pe-m)))))
              (if (and (bufferp target-orig) (buffer-live-p target-orig)
                       (markerp ob-m) (markerp oe-m)
                       (eq (marker-buffer ob-m) target-orig)
                       (eq (marker-buffer oe-m) target-orig))
                  (save-excursion
                    (delete-region sb se)
                    (goto-char sb)
                    (pcase-let ((`(,nb . ,ne)
                                 (macher-agent--insert-slice-with-shadow-props
                                  wbuf target-orig
                                  (marker-position ob-m)
                                  (marker-position oe-m)
                                  kind)))
                      (when (< nb ne)
                        (add-text-properties nb ne span-props))))
                (add-text-properties sb se span-props))
              (setq updated t)))))
      (when (and updated (bufferp target-orig) (buffer-live-p target-orig))
        (run-hook-with-args 'macher-agent-wire-invalidated-hook target-orig 'updated)))))

(defun macher-agent--insert-shadow-gap-span (wbuf orig-buf beg end insert-pos entries turn-id &optional kind backend-type)
  "Insert untracked ORIG-BUF `[BEG, END)' gap span into WBUF at INSERT-POS.
Uses ENTRIES, TURN-ID (or `(gap . span-id)' when nil), KIND, and BACKEND-TYPE.
Fires `macher-agent-wire-invalidated-hook' with `updated'."
  (when (and (bufferp wbuf) (buffer-live-p wbuf)
             (bufferp orig-buf) (buffer-live-p orig-buf)
             entries (< beg end))
    (let* ((ob-m (with-current-buffer orig-buf
                   (let ((m (set-marker (make-marker)
                                        (max (point-min) (min (point-max) beg))
                                        orig-buf)))
                     (set-marker-insertion-type m nil)
                     m)))
           (oe-m (with-current-buffer orig-buf
                   (let ((m (set-marker (make-marker)
                                        (max (point-min) (min (point-max) end))
                                        orig-buf)))
                     (set-marker-insertion-type m nil)
                     m)))
           (norm-h (macher-agent--wire-norm-hash orig-buf beg end))
           (nonempty (not (eql norm-h (sxhash-equal ""))))
           (copied (macher-agent--deep-copy-wire-data entries))
           (eff-kind (or kind 'gap))
           (chars (macher-agent--wire-estimate-chars copied)))
      (with-current-buffer wbuf
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (save-excursion
            (goto-char (max (point-min) (min (point-max) (or insert-pos (point-max)))))
            (unless (or (bobp) (eq (char-before) ?\n))
              (let ((nl-pt (point)))
                (insert "\n")
                (set-text-properties nl-pt (point) nil)))
            (setq-local macher-agent--wire-span-counter
                        (1+ macher-agent--wire-span-counter))
            (let* ((sid macher-agent--wire-span-counter)
                   (eff-tid (or turn-id (cons 'gap sid))))
              (pcase-let ((`(,nb . ,ne)
                           (macher-agent--insert-slice-with-shadow-props
                            wbuf orig-buf beg end eff-kind)))
                (when (< nb ne)
                  (add-text-properties
                   nb ne
                   (list 'macher-agent-span-id sid
                         'macher-agent-turn-id eff-tid
                         'macher-agent-span-kind eff-kind
                         'macher-agent-wire-entries copied
                         'macher-agent-wire-backend-type backend-type
                         'macher-agent-wire-chars chars
                         'macher-agent-orig-beg ob-m
                         'macher-agent-orig-end oe-m
                         'macher-agent-orig-nonempty nonempty
                         'macher-agent-norm-hash norm-h))))))))
      (run-hook-with-args 'macher-agent-wire-invalidated-hook orig-buf 'updated))))

(defun macher-agent--group-wire-spans-by-turn (spans)
  "Group ordered SPANS list into chronological `(TURN-ID . SPAN-LIST)' cells."
  (let ((groups nil))
    (dolist (sp spans)
      (let* ((tid (plist-get sp :turn-id))
             (cell (assoc tid groups)))
        (if cell
            (setcdr cell (append (cdr cell) (list sp)))
          (push (cons tid (list sp)) groups))))
    (nreverse groups)))

(defun macher-agent--span-live-in-buf-p (sp buf)
  "Return non-nil when SPAN SP has live markers attached to BUF."
  (let ((bm (plist-get sp :orig-beg))
        (em (plist-get sp :orig-end)))
    (and (markerp bm) (markerp em)
         (eq (marker-buffer bm) buf)
         (eq (marker-buffer em) buf))))

(defun macher-agent--span-deleted-p (sp buf)
  "Return non-nil when SPAN SP was deleted or cleared to blank in BUF."
  (or (not (macher-agent--span-live-in-buf-p sp buf))
      (and (plist-get sp :nonempty)
           (let ((b (marker-position (plist-get sp :orig-beg)))
                 (e (marker-position (plist-get sp :orig-end))))
             (or (>= b e)
                 (eql (macher-agent--wire-norm-hash buf b e)
                      (sxhash-equal "")))))))

(defun macher-agent--format-truncation-alert-prompt (backend lines-deleted toolless)
  "Build synthetic user alert prompt for BACKEND after truncating LINES-DELETED.
Adjusts wording when TOOLLESS is non-nil."
  (let ((msg (if toolless
                 (format "[... SYSTEM ALERT: macher-agent truncated %d lines of early history to conserve tokens ...]"
                         lines-deleted)
               (format "[... SYSTEM ALERT: macher-agent truncated %d lines of early history to conserve tokens. If you need this context, use the `search_conversation_history` tool ...]"
                       lines-deleted))))
    (or (and backend
             (fboundp 'gptel--parse-list)
             (let ((gptel-backend backend)
                   (gptel-cache nil))
               (car (ignore-errors (gptel--parse-list backend (list msg))))))
        (list :role "user" :content msg))))

(defun macher-agent--restore-response-props (buf beg end)
  "Restore `gptel' `response' property on unpropertised text in BUF `[BEG, END)'."
  (when (and (bufferp buf) (buffer-live-p buf))
    (with-current-buffer buf
      (with-silent-modifications
        (let ((p (max (point-min) beg))
              (lim (min (point-max) end)))
          (while (< p lim)
            (let ((gp (get-text-property p 'gptel))
                  (nx (next-single-property-change p 'gptel buf lim)))
              (unless gp
                (add-text-properties
                 p nx '(gptel response rear-nonsticky (gptel))))
              (setq p nx))))))))

(defun macher-agent--sync-shadow-spans (orig-buf limit-pos effective-backend &optional id-tables-cell)
  "Synchronise shadow wire spans of ORIG-BUF up to LIMIT-POS.
Prunes deleted spans, updates edited spans in place, and materialises
manual gap insertions for EFFECTIVE-BACKEND into `wbuf'.  Optionally
records parsed ID tables in ID-TABLES-CELL."
  (when (and (bufferp orig-buf) (buffer-live-p orig-buf))
    (let* ((wbuf (macher-agent--wire-buffer-for orig-buf nil))
           (target-btype (and effective-backend (type-of effective-backend)))
           (spans (and wbuf (macher-agent--collect-wire-spans wbuf)))
           (groups (macher-agent--group-wire-spans-by-turn spans))
           (to-prune nil)
           (prev-orig-pos (with-current-buffer orig-buf (point-min)))
           (stop-after-limit nil))
      (dolist (grp groups)
        (let* ((tid (car grp))
               (gspans (cdr grp))
               (u-sp (cl-find 'user-prompt gspans :key (lambda (s) (plist-get s :kind))))
               (t-sp (cl-find 'tool-step gspans :key (lambda (s) (plist-get s :kind))))
               (f-sp (cl-find 'model-final gspans :key (lambda (s) (plist-get s :kind))))
               (g-sp (cl-find 'gap gspans :key (lambda (s) (plist-get s :kind))))
               (standalone-gap-p (and g-sp (not u-sp) (not t-sp) (not f-sp)))
               (anchor-sp (or u-sp t-sp f-sp g-sp)))
          (cond
           ((not (and anchor-sp (macher-agent--span-live-in-buf-p anchor-sp orig-buf)))
            (setq to-prune (append to-prune gspans)))
           ((and u-sp (macher-agent--span-deleted-p u-sp orig-buf))
            (setq to-prune (append to-prune gspans)))
           ((and standalone-gap-p (macher-agent--span-deleted-p g-sp orig-buf))
            (setq to-prune (append to-prune gspans)))
           ((>= (marker-position (plist-get anchor-sp :orig-beg)) limit-pos)
            (dolist (sp gspans)
              (when (macher-agent--span-deleted-p sp orig-buf)
                (push sp to-prune)))
            (setq stop-after-limit t))
           ((not stop-after-limit)
            (let ((f-deleted (and f-sp (macher-agent--span-deleted-p f-sp orig-buf))))
              (dolist (sp gspans)
                (let* ((sp-kind (plist-get sp :kind))
                       (sp-live (macher-agent--span-live-in-buf-p sp orig-buf))
                       (sp-deleted
                        (or (not sp-live)
                            (macher-agent--span-deleted-p sp orig-buf)
                            (and (eq sp-kind 'tool-step)
                                 f-deleted
                                 (not (plist-get sp :nonempty)))))
                       (sp-beg (and sp-live (marker-position (plist-get sp :orig-beg))))
                       (sp-end (and sp-live (marker-position (plist-get sp :orig-end)))))
                  (cond
                   (sp-deleted
                    (push sp to-prune))
                   ((or stop-after-limit
                        (and (not (eq sp-kind 'user-prompt))
                             (not standalone-gap-p)
                             (> sp-beg limit-pos)))
                    (setq stop-after-limit t))
                   (t
                    (let* ((sp-end-c (min limit-pos sp-end))
                           (is-prompt-like (memq sp-kind '(user-prompt gap)))
                           (gap-tid (unless (or standalone-gap-p (eq sp-kind 'user-prompt))
                                      tid)))
                      (when (> sp-beg prev-orig-pos)
                        (when-let* ((gap-prompts
                                     (macher-agent--parse-buffer-slice
                                      orig-buf prev-orig-pos sp-beg effective-backend t orig-buf id-tables-cell))
                                    (ins-pos (or (car-safe (macher-agent--find-shadow-span-bounds
                                                            wbuf (plist-get sp :span-id)))
                                                 (plist-get sp :sbeg))))
                          (macher-agent--insert-shadow-gap-span
                           wbuf orig-buf prev-orig-pos sp-beg ins-pos gap-prompts gap-tid 'gap target-btype))
                        (setq prev-orig-pos sp-beg))
                      (when (and (= sp-end-c sp-end)
                                 (eq (plist-get sp :backend-type) target-btype))
                        (let ((cur-hash (macher-agent--wire-norm-hash orig-buf sp-beg sp-end-c)))
                          (unless (eql cur-hash (plist-get sp :norm-hash))
                            (unless is-prompt-like
                              (macher-agent--restore-response-props orig-buf sp-beg sp-end-c))
                            (when-let* ((reparsed (macher-agent--parse-buffer-slice
                                                   orig-buf sp-beg sp-end-c
                                                   effective-backend is-prompt-like orig-buf id-tables-cell)))
                              (macher-agent--update-shadow-span
                               wbuf sp reparsed cur-hash target-btype orig-buf)))))
                      (setq prev-orig-pos (max prev-orig-pos sp-end-c))))))))))))
      (when (< prev-orig-pos limit-pos)
        (when-let* ((tail-gap (macher-agent--parse-buffer-slice
                               orig-buf prev-orig-pos limit-pos effective-backend t orig-buf id-tables-cell)))
          (unless wbuf
            (setq wbuf (macher-agent--wire-buffer-for orig-buf t)))
          (macher-agent--insert-shadow-gap-span
           wbuf orig-buf prev-orig-pos limit-pos (with-current-buffer wbuf (point-max))
           tail-gap nil 'gap target-btype)))
      (when (and wbuf to-prune)
        (macher-agent--prune-shadow-spans wbuf (delete-dups to-prune) orig-buf)))))

(defun macher-agent--canonical-tool-args (args)
  "Return canonicalised representation of tool ARGS with sorted keys."
  (let ((parsed
         (if (stringp args)
             (or (ignore-errors
                   (json-parse-string args :object-type 'plist
                                      :null-object :null :false-object :json-false))
                 (string-trim args))
           args)))
    (cl-labels
        ((canon (obj)
           (cond
            ((and (consp obj) (keywordp (car obj)))
             (let ((pairs nil)
                   (cur obj))
               (while (consp cur)
                 (let ((k (pop cur))
                       (v (when (consp cur) (pop cur))))
                   (push (cons k (canon v)) pairs)))
               (sort pairs (lambda (a b)
                             (string< (symbol-name (car a))
                                      (symbol-name (car b)))))))
            ((vectorp obj)
             (mapcar #'canon (append obj nil)))
            ((consp obj)
             (mapcar #'canon obj))
            (t obj))))
      (canon parsed))))

(defun macher-agent--collect-dedup-sites (entries)
  "Collect `(SIG . MUTATOR)' tool-result deduplication sites in ENTRIES."
  (let ((omitted "{\"status\": \"omitted\", \"reason\": \"duplicate\"}")
        (sites nil)
        (id-sigs (make-hash-table :test 'equal))
        (vec (vconcat entries))
        (i 0)
        (len (length entries)))
    (while (< i len)
      (let ((msg (aref vec i)))
        (when (listp msg)
          (cond
           ;; Gemini: model :parts (:functionCall) paired with next user :parts (:functionResponse)
           ((and (equal (plist-get msg :role) "model")
                 (vectorp (plist-get msg :parts))
                 (< (1+ i) len)
                 (listp (aref vec (1+ i)))
                 (equal (plist-get (aref vec (1+ i)) :role) "user")
                 (vectorp (plist-get (aref vec (1+ i)) :parts)))
            (let ((fc-parts
                   (cl-remove-if-not (lambda (p) (and (listp p) (plist-member p :functionCall)))
                                     (append (plist-get msg :parts) nil)))
                  (fr-parts
                   (cl-remove-if-not (lambda (p) (and (listp p) (plist-member p :functionResponse)))
                                     (append (plist-get (aref vec (1+ i)) :parts) nil))))
              (cl-loop for fc-p in fc-parts
                       for fr-p in fr-parts
                       do (let* ((fc (plist-get fc-p :functionCall))
                                 (fr (plist-get fr-p :functionResponse))
                                 (fname (and (listp fc) (plist-get fc :name)))
                                 (rname (and (listp fr) (plist-get fr :name))))
                            (when (and fname (or (null rname) (equal fname rname)))
                              (let ((sig (cons fname (macher-agent--canonical-tool-args
                                                      (plist-get fc :args))))
                                    (target-fr fr))
                                (push (cons sig
                                            (lambda ()
                                              (let ((resp (plist-get target-fr :response)))
                                                (when (listp resp)
                                                  (plist-put resp :content omitted)))))
                                      sites)))))))
           ;; Anthropic: assistant :content vector (tool_use) -> user :content vector (tool_result)
           ((and (equal (plist-get msg :role) "assistant")
                 (vectorp (plist-get msg :content)))
            (cl-loop for blk across (plist-get msg :content)
                     when (and (listp blk) (equal (plist-get blk :type) "tool_use"))
                     do (when-let* ((id (plist-get blk :id))
                                    (name (plist-get blk :name)))
                          (puthash id (cons name (macher-agent--canonical-tool-args
                                                  (plist-get blk :input)))
                                   id-sigs))))
           ((and (equal (plist-get msg :role) "user")
                 (vectorp (plist-get msg :content)))
            (cl-loop for blk across (plist-get msg :content)
                     when (and (listp blk) (equal (plist-get blk :type) "tool_result"))
                     do (when-let* ((uid (plist-get blk :tool_use_id))
                                    (sig (gethash uid id-sigs)))
                          (let ((target-blk blk))
                            (push (cons sig (lambda () (plist-put target-blk :content omitted)))
                                  sites)))))
           ;; OpenAI Chat: assistant :tool_calls -> :role "tool"
           ((and (equal (plist-get msg :role) "assistant")
                 (vectorp (plist-get msg :tool_calls)))
            (cl-loop for tc across (plist-get msg :tool_calls)
                     when (listp tc)
                     do (let* ((id (plist-get tc :id))
                               (fn (plist-get tc :function))
                               (name (and (listp fn) (plist-get fn :name)))
                               (args (and (listp fn) (plist-get fn :arguments))))
                          (when (and id name)
                            (puthash id (cons name (macher-agent--canonical-tool-args args))
                                     id-sigs)))))
           ((equal (plist-get msg :role) "tool")
            (when-let* ((tid (plist-get msg :tool_call_id))
                        (sig (gethash tid id-sigs)))
              (let ((target-msg msg))
                (push (cons sig (lambda () (plist-put target-msg :content omitted)))
                      sites))))
           ;; OpenAI Responses: function_call -> function_call_output
           ((equal (plist-get msg :type) "function_call")
            (when-let* ((cid (plist-get msg :call_id))
                        (name (plist-get msg :name)))
              (puthash cid (cons name (macher-agent--canonical-tool-args
                                       (plist-get msg :arguments)))
                       id-sigs)))
           ((equal (plist-get msg :type) "function_call_output")
            (when-let* ((cid (plist-get msg :call_id))
                        (sig (gethash cid id-sigs)))
              (let ((target-msg msg))
                (push (cons sig (lambda () (plist-put target-msg :output omitted)))
                      sites)))))))
      (setq i (1+ i)))
    sites))

(defun macher-agent--deduplicate-wire-bundles (bundles up-to-turn-id)
  "Deduplicate older duplicate tool results in BUNDLES up to UP-TO-TURN-ID.
Deep-copies modified bundle `:entries' without mutating shadow buffer
properties and returns the updated bundle list."
  (if (or (null bundles)
          (null up-to-turn-id)
          (not (integerp up-to-turn-id))
          (null macher-agent-max-duplicate-tools)
          (<= macher-agent-max-duplicate-tools 0))
      bundles
    (let ((seen-tools (make-hash-table :test 'equal))
          (copied-bundles
           (mapcar (lambda (b)
                     (let ((tid (plist-get b :turn-id)))
                       (if (and (integerp tid) (<= tid up-to-turn-id))
                           (let ((nb (copy-sequence b)))
                             (plist-put nb :entries
                                        (macher-agent--deep-copy-wire-data
                                         (plist-get b :entries))))
                         b)))
                   bundles)))
      (dolist (b (reverse copied-bundles))
        (let ((tid (plist-get b :turn-id)))
          (when (and (integerp tid) (<= tid up-to-turn-id))
            (let ((sites (macher-agent--collect-dedup-sites (plist-get b :entries)))
                  (dirty nil))
              (dolist (site sites)
                (let* ((sig (car site))
                       (mutator (cdr site))
                       (cnt (gethash sig seen-tools 0)))
                  (if (>= cnt macher-agent-max-duplicate-tools)
                      (progn
                        (funcall mutator)
                        (setq dirty t))
                    (puthash sig (1+ cnt) seen-tools))))
              (when dirty
                (plist-put b :chars
                           (macher-agent--wire-estimate-chars
                            (plist-get b :entries))))))))
      copied-bundles)))

(defun macher-agent--build-wire-turn-bundles (wbuf &optional orig-buf limit-pos effective-backend id-tables-cell)
  "Build chronological turn bundles from WBUF up to LIMIT-POS in ORIG-BUF.
Each bundle is a plist `(:turn-id :entries :chars :sbeg :send)'.  When
EFFECTIVE-BACKEND is nil, reuses stored span entries directly and passes
optional ID-TABLES-CELL when re-parsing slices."
  (when (and (bufferp wbuf) (buffer-live-p wbuf))
    (let* ((spans (macher-agent--collect-wire-spans wbuf))
           (groups (macher-agent--group-wire-spans-by-turn spans))
           (target-btype (and effective-backend (type-of effective-backend)))
           (bundles nil)
           (stop-after-limit nil))
      (dolist (grp groups)
        (let* ((tid (car grp))
               (gspans (cdr grp))
               (u-sp (cl-find 'user-prompt gspans :key (lambda (s) (plist-get s :kind))))
               (t-sp (cl-find 'tool-step gspans :key (lambda (s) (plist-get s :kind))))
               (f-sp (cl-find 'model-final gspans :key (lambda (s) (plist-get s :kind))))
               (g-sp (cl-find 'gap gspans :key (lambda (s) (plist-get s :kind))))
               (anchor-sp (or u-sp t-sp f-sp g-sp))
               (anchor-beg (and anchor-sp (marker-position (plist-get anchor-sp :orig-beg)))))
          (cond
           ((not (and anchor-sp
                      (or (null orig-buf)
                          (macher-agent--span-live-in-buf-p anchor-sp orig-buf))))
            nil)
           ((and limit-pos anchor-beg (>= anchor-beg limit-pos))
            (setq stop-after-limit t))
           ((not stop-after-limit)
            (let ((turn-entries nil)
                  (turn-chars 0)
                  (turn-sbeg (plist-get (car gspans) :sbeg))
                  (turn-send (plist-get (car (last gspans)) :send)))
              (dolist (sp gspans)
                (let* ((sp-kind (plist-get sp :kind))
                       (sp-ob (plist-get sp :orig-beg))
                       (sp-oe (plist-get sp :orig-end))
                       (ob-pos (and (markerp sp-ob) (marker-position sp-ob)))
                       (oe-pos (and (markerp sp-oe) (marker-position sp-oe)))
                       (oe-clamped (if (and limit-pos oe-pos) (min limit-pos oe-pos) oe-pos))
                       (deleted (and orig-buf (macher-agent--span-deleted-p sp orig-buf))))
                  (when (and (not deleted)
                             (or (null limit-pos) (null ob-pos) (<= ob-pos limit-pos)))
                    (let* ((same-b (or (null target-btype)
                                       (eq (plist-get sp :backend-type) target-btype)))
                           (full-slice (or (null limit-pos) (null oe-pos) (= oe-clamped oe-pos)))
                           (hash-ok (or (null orig-buf)
                                        (null ob-pos)
                                        (null oe-clamped)
                                        (eql (macher-agent--wire-norm-hash orig-buf ob-pos oe-clamped)
                                             (plist-get sp :norm-hash)))))
                      (if (and same-b full-slice hash-ok)
                          (progn
                            (setq turn-entries
                                  (append turn-entries
                                          (macher-agent--deep-copy-wire-data
                                           (plist-get sp :entries))))
                            (setq turn-chars (+ turn-chars (or (plist-get sp :chars) 0))))
                        (when (and orig-buf effective-backend ob-pos oe-clamped)
                          (when (memq sp-kind '(tool-step model-final))
                            (macher-agent--restore-response-props orig-buf ob-pos oe-clamped))
                          (let ((reparsed
                                 (macher-agent--parse-buffer-slice
                                  orig-buf ob-pos oe-clamped effective-backend
                                  (not (memq sp-kind '(tool-step model-final)))
                                  orig-buf id-tables-cell)))
                            (setq turn-entries (append turn-entries reparsed))
                            (setq turn-chars
                                  (+ turn-chars
                                     (macher-agent--wire-estimate-chars reparsed))))))))))
              (when turn-entries
                (push (list :turn-id tid
                            :entries turn-entries
                            :chars turn-chars
                            :sbeg turn-sbeg
                            :send turn-send)
                      bundles)))))))
      (nreverse bundles))))

(defun macher-agent--resolve-wire-horizon (orig-buf bundles max-chars)
  "Resolve quantised truncation horizon and dedup epoch for ORIG-BUF.
Inspects chronological BUNDLES against MAX-CHARS and returns
`(:retained :horizon-send :lines-deleted)'."
  (if (or (null bundles) (null max-chars) (<= max-chars 0))
      (list :retained bundles :horizon-send nil :lines-deleted nil)
    (let* ((wbuf (macher-agent--wire-buffer-for orig-buf nil))
           (total-all (cl-loop for b in bundles sum (or (plist-get b :chars) 0)))
           (horizon (buffer-local-value 'macher-agent--wire-horizon-turn-id orig-buf))
           (pinned-lines (buffer-local-value 'macher-agent--wire-horizon-lines-deleted orig-buf))
           (dedup-tid (buffer-local-value 'macher-agent--wire-dedup-turn-id orig-buf))
           (match-horizon-p
            (lambda (tid h)
              (cond
               ((integerp h) (and (integerp tid) (<= tid h)))
               ((consp h) (equal tid h))
               (t nil)))))
      (when (or (<= total-all max-chars)
                (and horizon
                     (not (cl-some (lambda (b)
                                     (funcall match-horizon-p (plist-get b :turn-id) horizon))
                                   bundles))))
        (setq horizon nil
              pinned-lines nil
              dedup-tid nil)
        (with-current-buffer orig-buf
          (setq-local macher-agent--wire-horizon-turn-id nil)
          (setq-local macher-agent--wire-horizon-lines-deleted nil)
          (setq-local macher-agent--wire-dedup-turn-id nil)))
      ;; Step 1: drop prefix matching pinned horizon and record dropped-send
      (let ((retained (copy-sequence bundles))
            (dropped-send nil))
        (when horizon
          (let ((last-idx nil)
                (idx 0))
            (dolist (b retained)
              (when (funcall match-horizon-p (plist-get b :turn-id) horizon)
                (setq last-idx idx))
              (setq idx (1+ idx)))
            (when last-idx
              (let ((last-dropped (nth last-idx retained)))
                (setq dropped-send (plist-get last-dropped :send))
                (setq retained (nthcdr (1+ last-idx) retained))))))
        ;; Step 2: apply pinned deduplication boundary unconditionally
        (when (and dedup-tid
                   macher-agent-max-duplicate-tools
                   (> macher-agent-max-duplicate-tools 0))
          (setq retained (macher-agent--deduplicate-wire-bundles retained dedup-tid)))
        ;; Step 3: advance epoch only when remaining chars exceed max-chars
        (let ((rem (cl-loop for b in retained sum (or (plist-get b :chars) 0))))
          (when (> rem max-chars)
            (let ((max-int-tid
                   (cl-loop for b in retained
                            for tid = (plist-get b :turn-id)
                            when (integerp tid) maximize tid into mx
                            finally return (and (integerp mx) mx))))
              (when (and max-int-tid
                         macher-agent-max-duplicate-tools
                         (> macher-agent-max-duplicate-tools 0)
                         (not (eql max-int-tid dedup-tid)))
                (setq dedup-tid max-int-tid)
                (with-current-buffer orig-buf
                  (setq-local macher-agent--wire-dedup-turn-id dedup-tid))
                (setq retained (macher-agent--deduplicate-wire-bundles retained dedup-tid))
                (setq rem (cl-loop for b in retained sum (or (plist-get b :chars) 0)))))
            (when (> rem max-chars)
              (let* ((ratio (max 0.0 (min 0.99 (float (or macher-agent-context-compaction-ratio 0.25)))))
                     (target-chars (floor (* max-chars (- 1.0 ratio))))
                     (last-popped nil))
                (while (and (> rem target-chars) retained)
                  (let ((dropped (pop retained)))
                    (setq rem (- rem (or (plist-get dropped :chars) 0)))
                    (setq last-popped dropped)))
                (when last-popped
                  (setq dropped-send (plist-get last-popped :send))
                  (let* ((new-horizon (plist-get last-popped :turn-id))
                         (new-lines
                          (if (and wbuf (buffer-live-p wbuf) (integerp dropped-send))
                              (with-current-buffer wbuf
                                (max 1 (count-lines (point-min)
                                                    (max (point-min) (min (point-max) dropped-send)))))
                            1)))
                    (setq horizon new-horizon
                          pinned-lines new-lines)
                    (with-current-buffer orig-buf
                      (setq-local macher-agent--wire-horizon-turn-id new-horizon)
                      (setq-local macher-agent--wire-horizon-lines-deleted new-lines)
                      (setq-local macher-agent--wire-dedup-turn-id dedup-tid))
                    (run-hook-with-args 'macher-agent-wire-pre-truncate-hook orig-buf dropped-send)))))))
        (list :retained retained
              :horizon-send dropped-send
              :lines-deleted (or pinned-lines
                                 (buffer-local-value 'macher-agent--wire-horizon-lines-deleted orig-buf)))))))

(defun macher-agent--reconcile-wire-history (orig-buf limit-pos effective-backend disable-local-tools &optional max-turns max-chars id-tables-cell)
  "Reconcile shadow wire history of ORIG-BUF up to LIMIT-POS.
Formats prompts for EFFECTIVE-BACKEND, flattening tools when
DISABLE-LOCAL-TOOLS is non-nil, and bounding turns by MAX-TURNS and
MAX-CHARS.  Stores parsed ID tables in ID-TABLES-CELL when provided."
  (macher-agent--sync-shadow-spans orig-buf limit-pos effective-backend id-tables-cell)
  (let* ((wbuf (macher-agent--wire-buffer-for orig-buf nil))
         (target-btype (and effective-backend (type-of effective-backend)))
         (turn-bundles (and wbuf
                            (macher-agent--build-wire-turn-bundles
                             wbuf orig-buf limit-pos effective-backend id-tables-cell)))
         (horizon-res (macher-agent--resolve-wire-horizon orig-buf turn-bundles max-chars))
         (retained (plist-get horizon-res :retained))
         (horizon-send (plist-get horizon-res :horizon-send))
         (lines-del (plist-get horizon-res :lines-deleted)))
    (when (and (integerp max-turns) (> max-turns 0))
      (let ((keep (max 0 (1- max-turns))))
        (when (> (length retained) keep)
          (setq retained (last retained keep)))))
    (let* ((truncated-alert
            (when (and horizon-send lines-del)
              (macher-agent--format-truncation-alert-prompt
               effective-backend (max 1 lines-del) disable-local-tools)))
           (flat (cl-loop for b in retained append (plist-get b :entries))))
      (when truncated-alert
        (setq flat (cons truncated-alert flat)))
      (if (and disable-local-tools
               (eq target-btype 'gptel-gemini))
          (macher-agent--sanitize-prompts-for-toolless flat)
        flat))))

(defun macher-agent--build-prompts-from-buffer (buf backend tool-use &optional info)
  "Build BACKEND prompt list from BUF and open TOOL-USE calls using FSM INFO."
  (let* ((tracking (and (macher-agent--plist-p info) (plist-get info :tracking-marker)))
         (pos (and (macher-agent--plist-p info) (plist-get info :position)))
         (end-pos (when (and (bufferp buf) (buffer-live-p buf))
                    (or (and (markerp tracking) (eq (marker-buffer tracking) buf)
                             (marker-position tracking))
                        (and (markerp pos) (eq (marker-buffer pos) buf)
                             (marker-position pos)))))
         (id-tables-cell (list nil))
         (wbuf (and (bufferp buf) (buffer-live-p buf)
                    (macher-agent--wire-buffer-for buf nil)))
         (prompts
          (cond
           ((and wbuf
                 (macher-agent--wire-has-spans-p buf)
                 (eq backend (buffer-local-value 'macher-agent--wire-backend buf)))
            (let* ((pbounds (and (macher-agent--plist-p info)
                                 (plist-get info :macher-agent-prompt-bounds)))
                   (limit (or (car-safe pbounds)
                              (with-current-buffer buf
                                (macher-agent--prompt-start-pos (or end-pos (point-max))))))
                   (hist (macher-agent--reconcile-wire-history
                          buf limit backend nil
                          (with-current-buffer buf (bound-and-true-p gptel--num-messages-to-send))
                          nil id-tables-cell))
                   (active-tail
                    (macher-agent--parse-buffer-slice
                     buf limit (or end-pos (with-current-buffer buf (point-max)))
                     backend t buf id-tables-cell)))
              (append hist active-tail)))
           ((and (bufferp buf) (buffer-live-p buf)
                 (fboundp 'gptel--create-prompt-buffer))
            (with-current-buffer buf
              (save-excursion
                (goto-char (or end-pos (point-max)))
                (let* ((mark-active nil)
                       (copy-buf (gptel--create-prompt-buffer (point))))
                  (unwind-protect
                      (with-current-buffer copy-buf
                        (setcar id-tables-cell (macher-agent--assign-missing-buffer-tool-ids))
                        (setq-local gptel-track-response t)
                        (setq-local gptel-backend backend)
                        (goto-char (point-max))
                        (unless (string-blank-p
                                 (buffer-substring-no-properties (point-min) (point-max)))
                          (gptel--parse-buffer
                           backend
                           (and (bound-and-true-p gptel--num-messages-to-send)
                                (* 2 gptel--num-messages-to-send)))))
                    (when (buffer-live-p copy-buf)
                      (kill-buffer copy-buf)))))))))
         (id-tables (car id-tables-cell))
         (_ (macher-agent--ensure-tool-use-ids
             tool-use (car id-tables) (cdr id-tables)))
         (open-prompts
          (when (and tool-use (fboundp 'gptel--parse-list-and-insert))
            (macher-agent--merge-parallel-open-prompts
             (with-temp-buffer
               (setq-local gptel-track-response t)
               (setq-local gptel-backend backend)
               (let ((acc nil))
                 (dolist (tc tool-use (nreverse acc))
                   (erase-buffer)
                   (gptel--parse-list-and-insert
                    (list (cons 'tool
                                (list :id (plist-get tc :id)
                                      :name (plist-get tc :name)
                                      :args (plist-get tc :args)
                                      :result "pending"))))
                   (goto-char (point-max))
                   (dolist (p (butlast (gptel--parse-buffer backend nil)))
                     (push p acc)))))))))
    (append prompts open-prompts)))

(defun macher-agent--flatten-wire-tool-regions ()
  "Convert `(tool . _)' `gptel' properties in wire buffer to `response'.
Deletes `'ignore' spans so adjacent `'response' regions coalesce."
  (let ((inhibit-read-only t)
        (ignore-ranges nil)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((prop (get-char-property pos 'gptel))
            (next (next-single-property-change pos 'gptel nil (point-max))))
        (cond
         ((and (consp prop) (eq (car prop) 'tool))
          (put-text-property pos next 'gptel 'response))
         ((eq prop 'ignore)
          (push (cons pos next) ignore-ranges)))
        (setq pos next)))
    (dolist (rng ignore-ranges)
      (delete-region (car rng) (cdr rng)))))

(defun macher-agent--sanitize-prompts-for-toolless (prompts)
  "Flatten Gemini `:parts' tool structures in PROMPTS to plain text."
  (when (listp prompts)
    (macher-agent--merge-parallel-open-prompts
     (delq nil
           (mapcar
            (lambda (msg)
              (if (and (listp msg) (vectorp (plist-get msg :parts)))
                  (let* ((parts (plist-get msg :parts))
                         (changed nil)
                         (new-parts
                          (vconcat
                           (delq nil
                                 (mapcar
                                  (lambda (part)
                                    (cond
                                     ((and (listp part) (plist-get part :thought))
                                      (setq changed t)
                                      nil)
                                     ((and (listp part) (plist-member part :functionCall))
                                      (setq changed t)
                                      (let ((fc (plist-get part :functionCall)))
                                        (list :text
                                              (format "[Prior tool call %s(%S)]"
                                                      (or (plist-get fc :name) "tool")
                                                      (plist-get fc :args)))))
                                     ((and (listp part) (plist-member part :functionResponse))
                                      (setq changed t)
                                      (let* ((fr (plist-get part :functionResponse))
                                             (inner (plist-get fr :response))
                                             (content (if (listp inner)
                                                          (plist-get inner :content)
                                                        inner)))
                                        (list :text
                                              (format "[Prior tool result %s: %s]"
                                                      (or (plist-get fr :name) "tool")
                                                      (or content "")))))
                                     ((and (listp part) (plist-member part :thoughtSignature))
                                      (setq changed t)
                                      (let ((clean (copy-sequence part)))
                                        (cl-remf clean :thoughtSignature)
                                        (unless (string-empty-p (or (plist-get clean :text) ""))
                                          clean)))
                                     (t part)))
                                  parts)))))
                    (if changed
                        (when (> (length new-parts) 0)
                          (plist-put (copy-sequence msg) :parts new-parts))
                      msg))
                msg))
            prompts)))))

;;; Shadow Wire Buffer FSM Lifecycle

(defun macher-agent--apply-wire-prefix-to-fsm (fsm)
  "Splice pending `:macher-agent-wire-prefix' into FSM `:data' idempotently."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let ((info (gptel-fsm-info fsm)))
      (when (and (macher-agent--plist-p info)
                 (plist-member info :macher-agent-wire-prefix)
                 (listp (plist-get info :data))
                 (keywordp (car (plist-get info :data))))
        (let* ((prefix (plist-get info :macher-agent-wire-prefix))
               (backend (plist-get info :backend))
               (data (plist-get info :data))
               (new-info (copy-sequence info))
               (user-len (length (macher-agent--existing-prompts-from-data data))))
          (cl-remf new-info :macher-agent-wire-prefix)
          (when (and prefix backend (fboundp 'gptel--inject-prompt))
            (let* ((msgs (plist-get data :messages))
                   (inject-pos
                    (if (vectorp msgs)
                        (cl-loop for m across msgs
                                 while (and (listp m)
                                            (member (plist-get m :role) '("system" "developer")))
                                 count 1)
                      0)))
              (gptel--inject-prompt backend data (macher-agent--deep-copy-wire-data prefix) inject-pos)))
          (setq new-info (plist-put new-info :macher-agent-wire-prefix-len (length prefix)))
          (setq new-info (plist-put new-info :macher-agent-wire-user-len user-len))
          (macher-agent--set-fsm-info fsm new-info)
          (setf (gptel-fsm-info fsm) new-info)))))
  fsm)

(defun macher-agent--handle-wire-wait (fsm)
  "Prepare shadow wire state and callback wrapping on FSM entry to `WAIT'."
  (macher-agent--apply-wire-prefix-to-fsm fsm)
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (target-buf (and (macher-agent--plist-p info) (plist-get info :buffer))))
      (when (and (macher-agent--plist-p info)
                 (not (plist-get info :macher-agent-wire-turn-invalidated))
                 (macher-agent--wire-enabled-p target-buf info))
        (if (plist-get info :macher-agent-turn-started)
            (let* ((new-info (copy-sequence info))
                   (data (plist-get new-info :data))
                   (wait-len (length (macher-agent--existing-prompts-from-data data))))
              (dolist (k '(:reasoning-block :reasoning-marker :partial_reasoning
                                            :signature))
                (when (plist-member new-info k)
                  (setq new-info (plist-put new-info k nil))))
              (setq new-info (plist-put new-info :macher-agent-wire-wait-len wait-len))
              (macher-agent--set-fsm-info fsm new-info)
              (setf (gptel-fsm-info fsm) new-info))
          (let* ((wbuf (macher-agent--wire-buffer-for target-buf t))
                 (data (plist-get info :data))
                 (prefix-len (or (plist-get info :macher-agent-wire-prefix-len) 0))
                 (total-now (length (macher-agent--existing-prompts-from-data data)))
                 (user-len (or (plist-get info :macher-agent-wire-user-len)
                               (max 0 (- total-now prefix-len))))
                 (pbounds (plist-get info :macher-agent-prompt-bounds))
                 (pos-m (plist-get info :position))
                 (pbeg (or (car-safe pbounds)
                           (with-current-buffer target-buf
                             (macher-agent--prompt-start-pos
                              (or (and (markerp pos-m) (eq (marker-buffer pos-m) target-buf)
                                       (marker-position pos-m))
                                  (point-max))))))
                 (pend (or (cdr-safe pbounds)
                           (and (markerp pos-m) (eq (marker-buffer pos-m) target-buf)
                                (marker-position pos-m))
                           (with-current-buffer target-buf (point-max))))
                 (prompt-text
                  (or (plist-get info :macher-agent-clean-prompt-text)
                      (with-current-buffer target-buf
                        (let ((b (max (point-min) (min (point-max) pbeg)))
                              (e (max (point-min) (min (point-max) pend))))
                          (if (< b e) (buffer-substring-no-properties b e) "")))))
                 (pbeg-m (with-current-buffer target-buf
                           (let ((m (set-marker (make-marker)
                                                (max (point-min) (min (point-max) pbeg))
                                                target-buf)))
                             (set-marker-insertion-type m nil)
                             m)))
                 (pend-m (with-current-buffer target-buf
                           (let ((m (set-marker (make-marker)
                                                (max (point-min) (min (point-max) pend))
                                                target-buf)))
                             (set-marker-insertion-type m nil)
                             m)))
                 (shadow-start nil)
                 (shadow-uend nil)
                 (new-info (copy-sequence info)))
            (with-current-buffer wbuf
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (save-excursion
                  (goto-char (point-max))
                  (setq shadow-start (point))
                  (let ((txt (if (string-empty-p (string-trim prompt-text))
                                 "### \n\n"
                               (concat (string-trim-right prompt-text) "\n\n"))))
                    (insert txt)
                    (remove-text-properties shadow-start (point) '(gptel nil front-sticky nil)))
                  (setq shadow-uend (point)))))
            (setq new-info (plist-put new-info :macher-agent-turn-started t))
            (setq new-info (plist-put new-info :macher-agent-wire-prefix-len prefix-len))
            (setq new-info (plist-put new-info :macher-agent-wire-user-len user-len))
            (setq new-info (plist-put new-info :macher-agent-wire-wait-len total-now))
            (setq new-info (plist-put new-info :macher-agent-prompt-markers (cons pbeg-m pend-m)))
            (setq new-info (plist-put new-info :macher-agent-orig-tools-end nil))
            (setq new-info (plist-put new-info :macher-agent-wire-shadow-turn-start shadow-start))
            (setq new-info (plist-put new-info :macher-agent-wire-shadow-user-end shadow-uend))
            (setq new-info (plist-put new-info :macher-agent-wire-shadow-tools-end shadow-uend))
            (setq new-info (plist-put new-info :macher-agent-wire-shadow-final-written nil))
            (unless (plist-get new-info :macher-agent-cb-wrapped)
              (let* ((default-cb (or (plist-get new-info :callback)
                                     (if (and (plist-get new-info :stream)
                                              (bound-and-true-p gptel-use-curl))
                                         #'gptel-curl--stream-insert-response
                                       #'gptel--insert-response)))
                     (wrapped (macher-agent--make-wrapped-fsm-callback fsm target-buf default-cb)))
                (setq new-info (plist-put new-info :callback wrapped))
                (setq new-info (plist-put new-info :macher-agent-cb-wrapped t))))
            (macher-agent--set-fsm-info fsm new-info)
            (setf (gptel-fsm-info fsm) new-info)))))))

(defun macher-agent--ensure-final-wire-entry (fsm final-text)
  "Ensure FSM `:data' contains the final assistant message for FINAL-TEXT."
  (let* ((info (gptel-fsm-info fsm))
         (backend (and (macher-agent--plist-p info) (plist-get info :backend)))
         (data (and (macher-agent--plist-p info) (plist-get info :data)))
         (wait-len (or (plist-get info :macher-agent-wire-wait-len) 0))
         (cur-len (length (macher-agent--existing-prompts-from-data data))))
    (when (and backend
               (listp data)
               (keywordp (car data))
               (<= cur-len wait-len)
               (stringp final-text)
               (not (string-empty-p final-text))
               (fboundp 'gptel--parse-list)
               (fboundp 'gptel--inject-prompt))
      (let* ((gptel-backend backend)
             (gptel-cache nil)
             (parsed (ignore-errors (gptel--parse-list backend (list nil final-text))))
             (asst-msg (car parsed)))
        (when asst-msg
          (gptel--inject-prompt backend data asst-msg))))))

(defun macher-agent--extract-shadow-final-text (wbuf from-pos)
  "Extract `'response'-propertised text in shadow buffer WBUF from FROM-POS."
  (when (and (bufferp wbuf) (buffer-live-p wbuf) (integerp from-pos))
    (with-current-buffer wbuf
      (let ((pos (max (point-min) (min (point-max) from-pos)))
            (limit (point-max))
            (parts nil))
        (while (< pos limit)
          (let ((prop (get-text-property pos 'gptel))
                (next (next-single-property-change pos 'gptel wbuf limit)))
            (when (eq prop 'response)
              (push (buffer-substring-no-properties pos next) parts))
            (setq pos next)))
        (apply #'concat (nreverse parts))))))

(defun macher-agent--annotate-shadow-span (wbuf sbeg send props)
  "Attach PROPS plist with a fresh `macher-agent-span-id' on WBUF `[SBEG, SEND)'."
  (with-current-buffer wbuf
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (b (max (point-min) (min (point-max) sbeg)))
          (e (max (point-min) (min (point-max) send))))
      (when (<= b e)
        (when (= b e)
          (save-excursion
            (goto-char b)
            (insert "\n")
            (put-text-property b (point) 'gptel 'ignore)
            (setq e (point))))
        (setq-local macher-agent--wire-span-counter (1+ macher-agent--wire-span-counter))
        (add-text-properties
         b e
         (append (list 'macher-agent-span-id macher-agent--wire-span-counter)
                 props)))
      e)))

(defun macher-agent--capture-wire-done (fsm)
  "Commit lossless wire entries and markers for FSM into shadow wire buffer."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (target-buf (and (macher-agent--plist-p info) (plist-get info :buffer))))
      (when (and (macher-agent--plist-p info)
                 (plist-get info :macher-agent-turn-started)
                 (not (plist-get info :macher-agent-wire-turn-invalidated))
                 (not (plist-get info :macher-agent-wire-done-captured))
                 (macher-agent--wire-enabled-p target-buf info))
        (let* ((wbuf (macher-agent--wire-buffer-for target-buf t))
               (backend (plist-get info :backend))
               (btype (and backend (type-of backend)))
               (start-m (plist-get info :position))
               (track-m (or (plist-get info :tracking-marker) start-m))
               (rbeg (if (and (markerp start-m) (eq (marker-buffer start-m) target-buf))
                         (marker-position start-m)
                       (with-current-buffer target-buf (point-max))))
               (rend (if (and (markerp track-m) (eq (marker-buffer track-m) target-buf))
                         (marker-position track-m)
                       rbeg))
               (s-start (plist-get info :macher-agent-wire-shadow-turn-start))
               (s-uend (plist-get info :macher-agent-wire-shadow-user-end))
               (s-tend (plist-get info :macher-agent-wire-shadow-tools-end))
               (vis-text (with-current-buffer target-buf
                           (let ((b (max (point-min) (min (point-max) rbeg)))
                                 (e (max (point-min) (min (point-max) rend))))
                             (if (< b e)
                                 (macher-agent--extract-turn-response-text b e)
                               ""))))
               (raw-md-text (when (plist-get info :macher-agent-wire-shadow-final-written)
                              (macher-agent--extract-shadow-final-text wbuf s-tend)))
               (wire-final-text (if (and (stringp raw-md-text)
                                         (not (string-empty-p raw-md-text)))
                                    raw-md-text
                                  vis-text)))
          (macher-agent--shadow-close-reasoning-fence wbuf fsm)
          (macher-agent--ensure-final-wire-entry fsm wire-final-text)
          (let* ((data (plist-get (gptel-fsm-info fsm) :data))
                 (all-prompts (macher-agent--existing-prompts-from-data data))
                 (total-len (length all-prompts))
                 (prefix-len (min total-len
                                  (or (plist-get info :macher-agent-wire-prefix-len) 0)))
                 (user-len (min (- total-len prefix-len)
                                (or (plist-get info :macher-agent-wire-user-len) 0)))
                 (tools-start (+ prefix-len user-len))
                 (wait-len (max tools-start
                                (min total-len
                                     (or (plist-get info :macher-agent-wire-wait-len)
                                         tools-start))))
                 (user-entries (macher-agent--deep-copy-wire-data
                                (seq-subseq all-prompts prefix-len tools-start)))
                 (tool-entries (macher-agent--deep-copy-wire-data
                                (seq-subseq all-prompts tools-start wait-len)))
                 (final-entries (macher-agent--deep-copy-wire-data
                                 (nthcdr wait-len all-prompts)))
                 (orig-tend (cond
                             ((and tool-entries final-entries)
                              (max rbeg (min rend
                                             (or (plist-get info :macher-agent-orig-tools-end)
                                                 rbeg))))
                             (tool-entries rend)
                             (t rbeg)))
                 (pmarkers (plist-get info :macher-agent-prompt-markers))
                 (pbeg-m (car-safe pmarkers))
                 (pend-m (cdr-safe pmarkers))
                 (rbeg-m (with-current-buffer target-buf
                           (let ((m (set-marker (make-marker)
                                                (max (point-min) (min (point-max) rbeg))
                                                target-buf)))
                             (set-marker-insertion-type m nil)
                             m)))
                 (orig-tend-m (with-current-buffer target-buf
                                (let ((m (set-marker (make-marker)
                                                     (max (point-min) (min (point-max) orig-tend))
                                                     target-buf)))
                                  (set-marker-insertion-type m nil)
                                  m)))
                 (rend-m (with-current-buffer target-buf
                           (let ((m (set-marker (make-marker)
                                                (max (point-min) (min (point-max) rend))
                                                target-buf)))
                             (set-marker-insertion-type m nil)
                             m)))
                 (u-hash (and pbeg-m pend-m
                              (macher-agent--wire-norm-hash target-buf pbeg-m pend-m)))
                 (u-nonempty (and u-hash (not (eql u-hash (sxhash-equal "")))))
                 (t-hash (macher-agent--wire-norm-hash target-buf rbeg-m orig-tend-m))
                 (t-nonempty (not (eql t-hash (sxhash-equal ""))))
                 (f-hash (macher-agent--wire-norm-hash target-buf orig-tend-m rend-m))
                 (f-nonempty (not (eql f-hash (sxhash-equal ""))))
                 (r-nonempty (or t-nonempty f-nonempty))
                 (turn-id (with-current-buffer target-buf
                            (setq-local macher-agent--wire-backend backend)
                            (setq-local macher-agent--wire-turn-counter
                                        (1+ macher-agent--wire-turn-counter)))))
            (when (not r-nonempty)
              (with-current-buffer target-buf
                (let ((p (marker-position rend-m)))
                  (when (and (> p (point-min)) (eq (char-before p) ?\n))
                    (with-silent-modifications
                      (add-text-properties
                       (1- p) p '(gptel ignore rear-nonsticky (gptel))))))))
            (when (and (= prefix-len 0)
                       (with-current-buffer target-buf
                         (and (bound-and-true-p gptel-context)
                              (bound-and-true-p gptel-use-context)))
                       (fboundp 'gptel-context--collect-media))
              (with-current-buffer target-buf
                (when-let* ((mlist (ignore-errors (gptel-context--collect-media))))
                  (setq-local macher-agent--wire-last-media-hash (sxhash-equal mlist)))))
            (with-current-buffer wbuf
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (when (and final-entries
                           (not (plist-get info :macher-agent-wire-shadow-final-written))
                           (not (string-empty-p wire-final-text)))
                  (save-excursion
                    (goto-char (point-max))
                    (let ((pt (point)))
                      (insert wire-final-text)
                      (put-text-property pt (point) 'gptel 'response))))
                (save-excursion
                  (goto-char (point-max))
                  (unless (eq (char-before) ?\n)
                    (let ((pt (point)))
                      (insert "\n\n")
                      (put-text-property pt (point) 'gptel 'ignore))))))
            (when (and user-entries (integerp s-start) (integerp s-uend) pbeg-m pend-m)
              (let ((new-uend
                     (macher-agent--annotate-shadow-span
                      wbuf s-start s-uend
                      (list 'macher-agent-turn-id turn-id
                            'macher-agent-span-kind 'user-prompt
                            'macher-agent-wire-entries user-entries
                            'macher-agent-wire-backend-type btype
                            'macher-agent-wire-chars (macher-agent--wire-estimate-chars user-entries)
                            'macher-agent-orig-beg pbeg-m
                            'macher-agent-orig-end pend-m
                            'macher-agent-orig-nonempty u-nonempty
                            'macher-agent-norm-hash u-hash))))
                (when (= s-tend s-uend)
                  (setq s-tend new-uend))
                (setq s-uend new-uend)))
            (when (and tool-entries (integerp s-uend) (integerp s-tend))
              (setq s-tend
                    (macher-agent--annotate-shadow-span
                     wbuf s-uend s-tend
                     (list 'macher-agent-turn-id turn-id
                           'macher-agent-span-kind 'tool-step
                           'macher-agent-wire-entries tool-entries
                           'macher-agent-wire-backend-type btype
                           'macher-agent-wire-chars (macher-agent--wire-estimate-chars tool-entries)
                           'macher-agent-orig-beg rbeg-m
                           'macher-agent-orig-end orig-tend-m
                           'macher-agent-prompt-beg pbeg-m
                           'macher-agent-prompt-end pend-m
                           'macher-agent-orig-nonempty t-nonempty
                           'macher-agent-norm-hash t-hash))))
            (when (and final-entries (integerp s-tend))
              (macher-agent--annotate-shadow-span
               wbuf s-tend (with-current-buffer wbuf (point-max))
               (list 'macher-agent-turn-id turn-id
                     'macher-agent-span-kind 'model-final
                     'macher-agent-wire-entries final-entries
                     'macher-agent-wire-backend-type btype
                     'macher-agent-wire-chars (macher-agent--wire-estimate-chars final-entries)
                     'macher-agent-orig-beg orig-tend-m
                     'macher-agent-orig-end rend-m
                     'macher-agent-prompt-beg pbeg-m
                     'macher-agent-prompt-end rend-m
                     'macher-agent-orig-nonempty f-nonempty
                     'macher-agent-norm-hash f-hash)))
            (let* ((done-info (copy-sequence (gptel-fsm-info fsm))))
              (setq done-info (plist-put done-info :macher-agent-wire-done-captured t))
              (when r-nonempty
                (setq done-info (plist-put done-info :macher-agent-response-end-marker rend-m)))
              (macher-agent--set-fsm-info fsm done-info)
              (setf (gptel-fsm-info fsm) done-info))))))))

(defun macher-agent--rollback-wire-turn (fsm)
  "Roll back uncommitted shadow wire turn output on FSM `ERRS' or `ABRT'."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (target-buf (and (macher-agent--plist-p info) (plist-get info :buffer))))
      (when (and (macher-agent--plist-p info)
                 (plist-get info :macher-agent-turn-started)
                 (not (plist-get info :macher-agent-wire-done-captured)))
        (when-let* ((pmarkers (plist-get info :macher-agent-prompt-markers)))
          (when (markerp (car-safe pmarkers)) (set-marker (car pmarkers) nil))
          (when (markerp (cdr-safe pmarkers)) (set-marker (cdr pmarkers) nil)))
        (when-let* ((wbuf (and target-buf (macher-agent--wire-buffer-for target-buf nil)))
                    (s-start (plist-get info :macher-agent-wire-shadow-turn-start)))
          (with-current-buffer wbuf
            (let ((inhibit-read-only t)
                  (inhibit-modification-hooks t)
                  (b (max (point-min) (min (point-max) s-start))))
              (when (< b (point-max))
                (macher-agent--wire-clear-buffer-markers wbuf b (point-max))
                (delete-region b (point-max))))))
        (let ((clean-info (plist-put (copy-sequence info)
                                     :macher-agent-turn-started nil)))
          (macher-agent--set-fsm-info fsm clean-info)
          (setf (gptel-fsm-info fsm) clean-info))))))

(defun macher-agent--clone-wire-buffer (parent-buf child-buf)
  "Clone shadow wire buffer from PARENT-BUF into CHILD-BUF with fresh markers."
  (when (and (bufferp parent-buf) (buffer-live-p parent-buf)
             (bufferp child-buf) (buffer-live-p child-buf)
             (macher-agent--wire-has-spans-p parent-buf))
    (when-let* ((parent-wbuf (macher-agent--wire-buffer-for parent-buf nil))
                (child-wbuf (macher-agent--wire-buffer-for child-buf t)))
      (let ((raw-copy (with-current-buffer parent-wbuf (buffer-string)))
            (turn-cnt (buffer-local-value 'macher-agent--wire-turn-counter parent-buf))
            (horizon-tid (buffer-local-value 'macher-agent--wire-horizon-turn-id parent-buf))
            (horizon-lines (buffer-local-value 'macher-agent--wire-horizon-lines-deleted parent-buf))
            (dedup-tid (buffer-local-value 'macher-agent--wire-dedup-turn-id parent-buf))
            (span-cnt (buffer-local-value 'macher-agent--wire-span-counter parent-wbuf))
            (media-h (buffer-local-value 'macher-agent--wire-last-media-hash parent-buf))
            (wire-b (buffer-local-value 'macher-agent--wire-backend parent-buf))
            (marker-map (make-hash-table :test 'eq)))
        (with-current-buffer child-buf
          (setq-local macher-agent--wire-turn-counter turn-cnt)
          (setq-local macher-agent--wire-horizon-turn-id horizon-tid)
          (setq-local macher-agent--wire-horizon-lines-deleted horizon-lines)
          (setq-local macher-agent--wire-dedup-turn-id dedup-tid)
          (setq-local macher-agent--wire-last-media-hash media-h)
          (setq-local macher-agent--wire-backend wire-b))
        (with-current-buffer child-wbuf
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (erase-buffer)
            (insert raw-copy)
            (setq-local macher-agent--wire-span-counter span-cnt)
            (let ((pos (point-min)))
              (while (< pos (point-max))
                (let ((next (next-single-property-change
                             pos 'macher-agent-span-id child-wbuf (point-max))))
                  (when (get-text-property pos 'macher-agent-span-id)
                    (dolist (prop '(macher-agent-orig-beg
                                    macher-agent-orig-end
                                    macher-agent-prompt-beg
                                    macher-agent-prompt-end))
                      (let ((pm (get-text-property pos prop)))
                        (when (and (markerp pm) (eq (marker-buffer pm) parent-buf))
                          (let ((cm (or (gethash pm marker-map)
                                        (with-current-buffer child-buf
                                          (let ((m (set-marker
                                                    (make-marker)
                                                    (min (point-max)
                                                         (max (point-min) (marker-position pm)))
                                                    child-buf)))
                                            (set-marker-insertion-type m (marker-insertion-type pm))
                                            (puthash pm m marker-map)
                                            m)))))
                            (put-text-property pos next prop cm)))))
                    (when-let* ((ents (get-text-property pos 'macher-agent-wire-entries)))
                      (put-text-property
                       pos next 'macher-agent-wire-entries
                       (macher-agent--deep-copy-wire-data ents))))
                  (setq pos next))))))))))

(defun macher-agent-clear-wire-buffer (&optional buffer)
  "Clear committed spans and markers in shadow wire buffer for BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local macher-agent--wire-last-media-hash nil)
        (setq-local macher-agent--wire-backend nil)
        (setq-local macher-agent--wire-horizon-turn-id nil)
        (setq-local macher-agent--wire-horizon-lines-deleted nil)
        (setq-local macher-agent--wire-dedup-turn-id nil)
        (when-let* ((fsm (bound-and-true-p macher-agent--active-fsm))
                    ((fboundp 'gptel-fsm-info))
                    (info (ignore-errors (gptel-fsm-info fsm)))
                    ((macher-agent--plist-p info)))
          (let ((was-started (plist-get info :macher-agent-turn-started)))
            (when-let* ((pmarkers (plist-get info :macher-agent-prompt-markers)))
              (when (markerp (car-safe pmarkers)) (set-marker (car pmarkers) nil))
              (when (markerp (cdr-safe pmarkers)) (set-marker (cdr pmarkers) nil)))
            (dolist (k '(:macher-agent-turn-started
                         :macher-agent-wire-prefix
                         :macher-agent-wire-prefix-len
                         :macher-agent-wire-user-len
                         :macher-agent-wire-wait-len
                         :macher-agent-prompt-markers
                         :macher-agent-orig-tools-end
                         :macher-agent-wire-shadow-turn-start
                         :macher-agent-wire-shadow-user-end
                         :macher-agent-wire-shadow-tools-end
                         :macher-agent-wire-shadow-final-written
                         :macher-agent-shadow-in-reasoning))
              (when (plist-member info k)
                (setq info (plist-put info k nil))))
            (when was-started
              (setq info (plist-put info :macher-agent-wire-turn-invalidated t)))
            (macher-agent--set-fsm-info fsm info)
            (setf (gptel-fsm-info fsm) info))))
      (when-let* ((wbuf (macher-agent--wire-buffer-for buf nil)))
        (macher-agent--wire-clear-buffer-markers wbuf)
        (with-current-buffer wbuf
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (erase-buffer))))
      (run-hook-with-args 'macher-agent-wire-invalidated-hook buf 'cleared))))

(defun macher-agent-inspect-wire-buffer (&optional buffer)
  "Display the hidden shadow wire buffer associated with BUFFER."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (wbuf (macher-agent--wire-buffer-for buf nil)))
    (if (not wbuf)
        (user-error "No shadow wire buffer recorded for %s" (buffer-name buf))
      (pop-to-buffer wbuf))))

(defun macher-agent--resolve-backend-url (backend model info)
  "Resolve URL string for BACKEND and MODEL given FSM INFO, or nil."
  (when (and backend (fboundp 'gptel-backend-p) (gptel-backend-p backend))
    (let* ((gptel-backend backend)
           (gptel-model (or model (plist-get info :model) (bound-and-true-p gptel-model)))
           (gptel-stream (plist-get info :stream))
           (effective-info (if (and (macher-agent--plist-p info)
                                    (eq (plist-get info :backend) backend))
                               info
                             (plist-put (copy-sequence (and (macher-agent--plist-p info) info))
                                        :backend backend)))
           (raw-url (gptel-backend-url backend)))
      (cond
       ((stringp raw-url) raw-url)
       ((functionp raw-url)
        (ignore-errors
          (condition-case nil
              (funcall raw-url effective-info)
            (wrong-number-of-arguments
             (funcall raw-url)))))
       (t nil)))))

(defun macher-agent--sync-fsm-request-data (fsm compiled-prompt normalized-tools &optional model backend prev-backend)
  "Synchronise `:data', `:stream', `:tool-use', and `:url' on FSM.
Rebuilds `:data' natively via `gptel--request-data' using COMPILED-PROMPT,
NORMALIZED-TOOLS, MODEL, and BACKEND, tracking transitions from PREV-BACKEND."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (is-plist (macher-agent--plist-p info))
           (data (when is-plist (plist-get info :data)))
           (info-backend (when is-plist (plist-get info :backend)))
           (effective-backend (when is-plist
                                (or backend
                                    info-backend
                                    (bound-and-true-p gptel-backend))))
           (effective-model (or model
                                (when is-plist (plist-get info :model))
                                (bound-and-true-p gptel-model)))
           (model-sym (cond ((symbolp effective-model) effective-model)
                            ((stringp effective-model) (intern effective-model))
                            (t effective-model)))
           (effective-prev-backend (or prev-backend info-backend))
           (backend-changed-p (and effective-backend effective-prev-backend
                                   (not (eq effective-backend effective-prev-backend))))
           (disable-local-tools (and is-plist (plist-get info :disable-local-tools)))
           (has-req-params (and is-plist (plist-member info :request-params)))
           (req-params (when has-req-params (plist-get info :request-params)))
           (effective-tools (unless disable-local-tools normalized-tools))
           (info-updated nil))
      (when is-plist
        (when backend-changed-p
          (let ((tbuf (plist-get info :buffer)))
            (when (and (bufferp tbuf) (buffer-live-p tbuf))
              (macher-agent-clear-wire-buffer tbuf)
              (setq info (gptel-fsm-info fsm)))))
        (when (and backend (not (eq backend info-backend)))
          (unless info-updated (setq info (copy-sequence info)))
          (setq info (plist-put info :backend backend)
                info-updated t))
        (when (and model (not (eq model (plist-get info :model))))
          (unless info-updated (setq info (copy-sequence info)))
          (setq info (plist-put info :model model)
                info-updated t))
        (when (and disable-local-tools (plist-get info :tools))
          (unless info-updated (setq info (copy-sequence info)))
          (setq info (plist-put info :tools nil)
                info-updated t))
        (cond
         ((and effective-tools
               (eq (bound-and-true-p gptel-use-tools) 'force)
               (not (eq (plist-get info :use-tools) 'force)))
          (unless info-updated (setq info (copy-sequence info)))
          (setq info (plist-put info :use-tools 'force)
                info-updated t))
         ((and (null effective-tools)
               (plist-member info :use-tools))
          (unless info-updated (setq info (copy-sequence info)))
          (cl-remf info :use-tools)
          (setq info-updated t)))
        (when (and effective-backend
                   (fboundp 'gptel-backend-p)
                   (gptel-backend-p effective-backend)
                   (fboundp 'gptel-backend-stream))
          (cond
           ((not (gptel-backend-stream effective-backend))
            (when (plist-member info :stream)
              (unless info-updated (setq info (copy-sequence info)))
              (cl-remf info :stream)
              (setq info-updated t)))
           ((and backend-changed-p
                 effective-prev-backend
                 (gptel-backend-p effective-prev-backend)
                 (not (gptel-backend-stream effective-prev-backend))
                 (gptel-backend-stream effective-backend)
                 (bound-and-true-p gptel-stream)
                 (bound-and-true-p gptel-use-curl)
                 (not (plist-get info :stream)))
            (let* ((model-params (when (fboundp 'gptel--model-request-params)
                                   (gptel--model-request-params model-sym)))
                   (stream-spec (plist-get model-params :stream)))
              (when (or (not (memq :stream model-params))
                        (and stream-spec (not (eq stream-spec :json-false))))
                (unless info-updated (setq info (copy-sequence info)))
                (setq info (plist-put info :stream t)
                      info-updated t)))))))
      (cond
       ((and (listp data)
             (keywordp (car data))
             effective-backend
             (fboundp 'gptel-backend-p)
             (gptel-backend-p effective-backend)
             (fboundp 'gptel--request-data))
        (let* ((gptel-backend effective-backend)
               (gptel-model model-sym)
               (gptel-system-prompt
                (when (and (stringp compiled-prompt)
                           (not (string-empty-p compiled-prompt)))
                  compiled-prompt))
               (gptel-tools effective-tools)
               (gptel-use-tools
                (if effective-tools
                    (if (or (eq (bound-and-true-p gptel-use-tools) 'force)
                            (eq (plist-get info :use-tools) 'force))
                        'force
                      (or (bound-and-true-p gptel-use-tools) t))
                  nil))
               (gptel--request-params
                (if has-req-params
                    req-params
                  (bound-and-true-p gptel--request-params)))
               (gptel-stream (plist-get info :stream))
               (gptel-temperature (or (plist-get info :temperature)
                                      (bound-and-true-p gptel-temperature)))
               (gptel-max-tokens (or (plist-get info :max-tokens)
                                     (bound-and-true-p gptel-max-tokens)))
               (raw-prompts
                (if backend-changed-p
                    (macher-agent--build-prompts-from-buffer
                     (plist-get info :buffer)
                     effective-backend
                     (unless disable-local-tools (plist-get info :tool-use))
                     info)
                  (or (macher-agent--existing-prompts-from-data data)
                      (macher-agent--build-prompts-from-buffer
                       (plist-get info :buffer)
                       effective-backend
                       (unless disable-local-tools (plist-get info :tool-use))
                       info))))
               (prompts
                (if (and disable-local-tools
                         (eq (type-of effective-backend) 'gptel-gemini))
                    (macher-agent--sanitize-prompts-for-toolless raw-prompts)
                  raw-prompts))
               (reencoded (condition-case nil
                              (gptel--request-data effective-backend prompts)
                            (error nil))))
          (when (and (listp reencoded) (keywordp (car reencoded)))
            (unless info-updated (setq info (copy-sequence info)))
            (setq info (plist-put info :data reencoded)
                  info-updated t))))
       ((and (null effective-backend)
             (listp data)
             (keywordp (car data))
             (stringp compiled-prompt)
             (plist-member data :system))
        (unless info-updated (setq info (copy-sequence info)))
        (setq info (plist-put info :data
                              (plist-put (copy-sequence data) :system compiled-prompt))
              info-updated t)))
      (when (and effective-backend
                 (fboundp 'gptel-backend-p)
                 (gptel-backend-p effective-backend)
                 (or backend-changed-p
                     (plist-member info :url)
                     (functionp (gptel-backend-url effective-backend))))
        (let ((resolved-url (macher-agent--resolve-backend-url
                             effective-backend effective-model info)))
          (if resolved-url
              (progn
                (unless info-updated
                  (setq info (copy-sequence info)))
                (setq info (plist-put info :url resolved-url)
                      info-updated t))
            (when (plist-member info :url)
              (unless info-updated
                (setq info (copy-sequence info)))
              (cl-remf info :url)
              (setq info-updated t)))))
      (when info-updated
        (macher-agent--set-fsm-info fsm info)))))

(defun macher-agent--resolve-active-preset-for-buffer (buffer-presets ctx target-buf)
  "Return the primary active preset symbol for TARGET-BUF, or nil.
Inspects BUFFER-PRESETS and CTX."
  (if buffer-presets
      (macher-agent--primary-preset-symbol buffer-presets ctx target-buf t)
    (bound-and-true-p gptel--preset)))

(defun macher-agent--recompile-and-sync-buffer-turn (target-buf &optional context)
  "Recompile transmission state for TARGET-BUF using CONTEXT and sync FSM.
Leaves the buffer prompt unmodified."
  (when (and (bufferp target-buf) (buffer-live-p target-buf))
    (with-current-buffer target-buf
      (let* ((ctx (or context (macher-agent-resolve-context target-buf)))
             (flv (bound-and-true-p file-local-variables-alist))
             (active-fsm (bound-and-true-p macher-agent--active-fsm))
             (fsm-info (when (and active-fsm (fboundp 'gptel-fsm-info))
                         (ignore-errors (gptel-fsm-info active-fsm))))
             (matching-fsm (when (and active-fsm
                                      (macher-agent--plist-p fsm-info)
                                      (eq (plist-get fsm-info :buffer) target-buf))
                             active-fsm))
             (has-live-request-data
              (when matching-fsm
                (let ((d (plist-get fsm-info :data)))
                  (and (listp d) (keywordp (car d))))))
             (buffer-presets (bound-and-true-p macher-agent-presets))
             (active-preset-sym (macher-agent--resolve-active-preset-for-buffer
                                 buffer-presets ctx target-buf))
             (active-preset-spec (when active-preset-sym
                                   (let ((sp (macher-agent--lookup-skill-spec
                                              active-preset-sym ctx target-buf)))
                                     (when (and sp (not (plist-get sp :is-command)))
                                       sp)))))
        (when (and (local-variable-p 'gptel-tools target-buf)
                   (listp gptel-tools)
                   gptel-tools)
          (setq-local gptel-tools
                      (mapcar (lambda (tl)
                                (macher-agent--refresh-tool-struct tl ctx))
                              gptel-tools)))
        (when (and active-preset-spec
                   (not (assq 'gptel-system-prompt flv)))
          (when-let* ((raw-sys (or (plist-get active-preset-spec :system)
                                   (plist-get active-preset-spec :system-message)))
                      ((stringp raw-sys)))
            (setq-local gptel-system-prompt raw-sys)))
        (let* ((known (bound-and-true-p gptel--known-presets))
               (use-skill-active (macher-agent--has-cached-skill-state-p target-buf))
               (fsm-inline-skills (when (and matching-fsm (not use-skill-active))
                                    (plist-get fsm-info :inline-skills)))
               (fsm-redirected-skill (when (and matching-fsm (not use-skill-active))
                                       (plist-get fsm-info :redirected-skill)))
               (has-ephemeral-inline (or fsm-inline-skills fsm-redirected-skill))
               (tx-skills (macher-agent--transformer-resolve-skills
                           buffer-presets fsm-inline-skills known ctx target-buf))
               (state (macher-agent--compile-transmission-payload
                       target-buf buffer-presets tx-skills fsm-redirected-skill ctx
                       (if has-live-request-data t :preserve)))
               (norm-tools (unless (macher-agent-transmission-state-disable-local-tools state)
                             (macher-agent-normalize-tools
                              (macher-agent-transmission-state-tools state))))
               (resolved-model (macher-agent-transmission-state-model state))
               (resolved-backend (macher-agent-transmission-state-backend state)))
          (setf (macher-agent-transmission-state-tools state) norm-tools)
          (unless has-ephemeral-inline
            (if (macher-agent-transmission-state-disable-local-tools state)
                (setq-local gptel-tools nil)
              (when use-skill-active
                (setq-local gptel-tools
                            (macher-agent--ensure-tool-in-list
                             gptel-tools "use_skill" ctx target-buf 'macher-agent-use-skill-tool))))
            (unless (assq 'gptel--request-params flv)
              (let ((rp (macher-agent-transmission-state-request-params state)))
                (if rp
                    (progn
                      (setq-local gptel--request-params rp)
                      (setq-local macher-agent--preset-request-params rp))
                  (kill-local-variable 'gptel--request-params)
                  (setq-local macher-agent--preset-request-params nil))))
            (setq-local macher-agent--disable-local-tools
                        (and (macher-agent-transmission-state-disable-local-tools state) t))
            (when (and resolved-model (not (assq 'gptel-model flv)))
              (setq-local gptel-model resolved-model))
            (when (and resolved-backend
                       (not (assq 'gptel-backend flv))
                       (not (assq 'gptel--backend-name flv)))
              (setq-local gptel-backend resolved-backend)))
          (when matching-fsm
            (macher-agent--update-fsm-info-from-transmission-state matching-fsm state))
          state)))))

(defun macher-agent-pipe--extract-redirect (state)
  "Route redirected skill text in STATE, merging tools and handling commands."
  (cl-check-type state macher-agent-transmission-state)
  (let ((redirected-skill (macher-agent-transmission-state-redirected-skill state))
        (target-buf (macher-agent-transmission-state-target-buffer state)))
    (when redirected-skill
      (let* ((known (if (and target-buf (buffer-live-p target-buf))
                        (with-current-buffer target-buf (bound-and-true-p gptel--known-presets))
                      (macher-agent-transmission-state-known-presets state)))
             (is-plist (listp redirected-skill))
             (ctx (macher-agent-transmission-state-context state))
             (spec (unless is-plist
                     (macher-agent--lookup-skill-spec
                      redirected-skill ctx target-buf known t)))
             (redirect-state (unless is-plist
                               (if (and target-buf (buffer-live-p target-buf))
                                   (with-current-buffer target-buf
                                     (macher-agent-compose-payload
                                      (list :known-presets known)
                                      (list redirected-skill)))
                                 (macher-agent-compose-payload
                                  (list :known-presets known)
                                  (list redirected-skill)))))
             (redirect-text (if is-plist
                                (plist-get redirected-skill :system)
                              (or (plist-get spec :system) (plist-get redirect-state :system))))
             (redirect-tools (if is-plist
                                 (plist-get redirected-skill :tools)
                               (plist-get redirect-state :tools)))
             (redirect-model (if is-plist
                                 (plist-get redirected-skill :model)
                               (or (plist-get spec :model)
                                   (plist-get redirect-state :model))))
             (redirect-backend (if is-plist
                                   (plist-get redirected-skill :backend)
                                 (or (plist-get spec :backend)
                                     (plist-get redirect-state :backend))))
             (redirect-req-params (if is-plist
                                      (plist-get redirected-skill :request-params)
                                    (or (plist-get spec :request-params)
                                        (plist-get redirect-state :request-params))))
             (redirect-disable-tools (if is-plist
                                         (plist-get redirected-skill :disable-local-tools)
                                       (or (plist-get spec :disable-local-tools)
                                           (plist-get redirect-state :disable-local-tools))))
             (is-command (if is-plist
                             (plist-get redirected-skill :is-command)
                           (plist-get spec :is-command))))
        (when (or redirect-req-params redirect-disable-tools)
          (setf (macher-agent-transmission-state-request-params state) redirect-req-params))
        (when redirect-disable-tools
          (setf (macher-agent-transmission-state-disable-local-tools state) t)
          (setf (macher-agent-transmission-state-tools state) nil)
          (setf (macher-agent-transmission-state-ptc-primitives state) nil))
        (when (or redirect-model redirect-backend)
          (pcase-let ((`(,resolved-backend . ,resolved-model)
                       (macher-agent--resolve-model-and-backend
                        (macher-agent-transmission-state-backend state)
                        (macher-agent-transmission-state-model state)
                        redirect-backend
                        redirect-model)))
            (when resolved-backend
              (setf (macher-agent-transmission-state-backend state) resolved-backend))
            (when resolved-model
              (setf (macher-agent-transmission-state-model state) resolved-model))))
        (when redirect-text
          (if is-command
              (let* ((raw-args (if ctx
                                   (macher-agent-context-prompt ctx)
                                 (let ((ps (macher-agent--prompt-start-pos)))
                                   (string-trim
                                    (buffer-substring-no-properties
                                     (macher-agent--skip-prompt-prefix ps)
                                     (point-max))))))
                     (cmd-name (if is-plist
                                   (symbol-name (or (plist-get redirected-skill :name-sym) redirected-skill))
                                 (symbol-name redirected-skill)))
                     (clean-args (if (and cmd-name raw-args)
                                     (replace-regexp-in-string
                                      (format "\\`[ \t\r\n#*>]*@%s\\b[ \t]*" (regexp-quote cmd-name)) "" raw-args)
                                   raw-args))
                     (interpolated (macher-agent--interpolate-command-body redirect-text clean-args)))
                (setf (macher-agent-transmission-state-redirect-prompt state) interpolated)
                (setf (macher-agent-transmission-state-compiled-prompt state)
                      (macher-agent-transmission-state-base-prompt state)))
            (setf (macher-agent-transmission-state-redirect-prompt state) redirect-text)
            (setf (macher-agent-transmission-state-base-prompt state) redirect-text)
            (setf (macher-agent-transmission-state-compiled-prompt state) redirect-text)))
        (when (and redirect-tools
                   (not (macher-agent-transmission-state-disable-local-tools state)))
          (setf (macher-agent-transmission-state-tools state)
                (append redirect-tools (macher-agent-transmission-state-tools state)))))))
  state)

(defun macher-agent-sync-prompt-transformer (async-fn fsm)
  "Synchronise VFS, extract prompt, and compose skills for FSM.
Invokes ASYNC-FN when transformation finishes."
  (let* ((temp-buf (current-buffer))
         (active-fsm (macher-agent-get-active-fsm fsm))
         (info (when-let* ((fsm-obj active-fsm)
                           ((fboundp 'gptel-fsm-info)))
                 (ignore-errors (gptel-fsm-info fsm-obj))))
         (orig-buf (or (and info (plist-get info :buffer)) temp-buf))
         (context (or (when active-fsm
                        (macher-agent-gptel-context-from-fsm active-fsm orig-buf))
                      (macher-agent-resolve-context orig-buf)))
         (_ (unless (eq temp-buf orig-buf)
              (macher-agent--strip-wire-local-variables)))
         (prompt-start
          (let ((base-ps (macher-agent--prompt-start-pos)))
            (if (and (not (eq temp-buf orig-buf))
                     (buffer-live-p orig-buf)
                     (macher-agent--wire-has-spans-p orig-buf))
                (let* ((pm (and (macher-agent--plist-p info) (plist-get info :position)))
                       (s-end (if (and (markerp pm) (eq (marker-buffer pm) orig-buf))
                                  (marker-position pm)
                                (with-current-buffer orig-buf (point-max))))
                       (o-beg (with-current-buffer orig-buf
                                (macher-agent--prompt-start-pos s-end)))
                       (o-tail (with-current-buffer orig-buf
                                 (let ((b (max (point-min) (min (point-max) o-beg)))
                                       (e (max (point-min) (min (point-max) s-end))))
                                   (and (<= b e) (buffer-substring-no-properties b e)))))
                       (t-str (buffer-substring-no-properties (point-min) (point-max))))
                  (if (and o-tail (string-suffix-p o-tail t-str))
                      (max base-ps (- (point-max) (length o-tail)))
                    base-ps))
              base-ps)))
         (prompt-start-marker (copy-marker prompt-start nil)))

    (when (buffer-live-p orig-buf)
      (with-current-buffer orig-buf
        (setq-local macher-agent--turn-submission-delivered nil)
        (when active-fsm
          (setq-local macher-agent--active-fsm active-fsm))
        (when context
          (macher-agent--transformer-sync-context orig-buf context)
          (when (macher-agent-valid-context-p context)
            (unless (local-variable-p 'gptel--known-presets orig-buf)
              (setq-local gptel--known-presets
                          (copy-alist (bound-and-true-p gptel--known-presets))))
            (unless (local-variable-p 'gptel-directives orig-buf)
              (setq-local gptel-directives
                          (copy-alist (bound-and-true-p gptel-directives))))
            (unless (local-variable-p 'gptel--known-tools orig-buf)
              (setq-local gptel--known-tools
                          (copy-tree (bound-and-true-p gptel--known-tools))))
            (ignore-errors (macher-agent-initialize-skills context)))
          (let ((raw-prompt (with-current-buffer temp-buf
                              (string-trim
                               (buffer-substring-no-properties
                                (macher-agent--skip-prompt-prefix prompt-start)
                                (point-max))))))
            (unless (string-empty-p raw-prompt)
              (setf (macher-agent-context-prompt context) raw-prompt)))
          (when-let* ((info-prompt (and info (plist-get info :prompt))))
            (let ((clean-info (replace-regexp-in-string "\\`[#>*[:space:]]+" "" (string-trim info-prompt))))
              (unless (string-empty-p clean-info)
                (setf (macher-agent-context-prompt context) clean-info)))))))

    (when (and active-fsm context)
      (macher-agent--set-fsm-info active-fsm
                                  (plist-put (gptel-fsm-info active-fsm) :macher-agent-context context)))

    (when (and active-fsm
               (not (eq temp-buf orig-buf))
               (buffer-live-p orig-buf)
               (macher-agent--wire-enabled-p orig-buf info)
               (eq (buffer-local-value 'major-mode temp-buf)
                   (buffer-local-value 'major-mode orig-buf))
               (not (with-current-buffer orig-buf (use-region-p))))
      (let ((prev-wire-backend (buffer-local-value 'macher-agent--wire-backend orig-buf))
            (cur-buf-backend (or (and info (plist-get info :backend))
                                 (buffer-local-value 'gptel-backend orig-buf))))
        (when (and prev-wire-backend cur-buf-backend
                   (not (eq cur-buf-backend prev-wire-backend)))
          (macher-agent-clear-wire-buffer orig-buf))
        (let* ((pm (and (macher-agent--plist-p info) (plist-get info :position)))
               (send-end (if (and (markerp pm) (eq (marker-buffer pm) orig-buf))
                             (marker-position pm)
                           (with-current-buffer orig-buf (point-max))))
               (orig-prompt-beg (with-current-buffer orig-buf
                                  (macher-agent--prompt-start-pos send-end)))
               (has-spans (macher-agent--wire-has-spans-p orig-buf))
               (has-untracked
                (and (not has-spans)
                     (not (plist-get info :dry-run))
                     (with-current-buffer orig-buf
                       (and (> orig-prompt-beg (point-min))
                            (not (string-blank-p
                                  (buffer-substring-no-properties
                                   (point-min) orig-prompt-beg))))))))
          (when (or has-spans has-untracked)
            (when (and has-untracked cur-buf-backend
                       (not (buffer-local-value 'macher-agent--wire-backend orig-buf)))
              (with-current-buffer orig-buf
                (setq-local macher-agent--wire-backend cur-buf-backend)))
            (macher-agent--sync-shadow-spans
             orig-buf orig-prompt-beg
             (or (buffer-local-value 'macher-agent--wire-backend orig-buf)
                 cur-buf-backend))))))

    (let* ((buffer-presets (when (buffer-live-p orig-buf)
                             (buffer-local-value 'macher-agent-presets orig-buf)))
           (known (when (buffer-live-p orig-buf)
                    (buffer-local-value 'gptel--known-presets orig-buf)))
           (extraction (macher-agent--extract-inline-skills prompt-start orig-buf))
           (inline-skills (car extraction))
           (inline-preset-used (cdr extraction))
           (transmission-skills (macher-agent--transformer-resolve-skills
                                 buffer-presets inline-skills known context orig-buf))
           (redirected-skill (macher-agent--transformer-detect-redirect
                              inline-preset-used inline-skills known context))
           (state (macher-agent--compile-transmission-payload
                   orig-buf buffer-presets transmission-skills redirected-skill context))
           (normalized-tools
            (unless (macher-agent-transmission-state-disable-local-tools state)
              (if (buffer-live-p orig-buf)
                  (with-current-buffer orig-buf
                    (macher-agent-normalize-tools (macher-agent-transmission-state-tools state)))
                (macher-agent-normalize-tools (macher-agent-transmission-state-tools state)))))
           (payload (list :model (macher-agent-transmission-state-model state)
                          :backend (or (macher-agent-transmission-state-backend state)
                                       (when (buffer-live-p orig-buf) (buffer-local-value 'gptel-backend orig-buf)))
                          :temperature (macher-agent-transmission-state-temperature state)
                          :max-tokens (macher-agent-transmission-state-max-tokens state)
                          :request-params (macher-agent-transmission-state-request-params state)
                          :disable-local-tools (macher-agent-transmission-state-disable-local-tools state)
                          :tools normalized-tools
                          :ptc-primitives (macher-agent-transmission-state-ptc-primitives state))))

      (setf (macher-agent-transmission-state-tools state) normalized-tools)
      (macher-agent--apply-payload-locally payload)
      (when (and (macher-agent-transmission-state-disable-local-tools state)
                 (not (eq temp-buf orig-buf)))
        (setq-local gptel-use-tools nil))

      (when-let* ((compiled (macher-agent-transmission-state-compiled-prompt state)))
        (setq-local gptel-system-prompt compiled))

      (when active-fsm
        (let ((inf (copy-sequence (gptel-fsm-info active-fsm))))
          (setq inf (plist-put inf :inline-skills inline-skills))
          (setq inf (plist-put inf :redirected-skill redirected-skill))
          (macher-agent--set-fsm-info active-fsm inf)
          (setf (gptel-fsm-info active-fsm) inf))
        (macher-agent--update-fsm-info-from-transmission-state active-fsm state))

      (when-let* ((redirect-text (macher-agent-transmission-state-redirect-prompt state)))
        (let ((replace-from (macher-agent--skip-prompt-prefix (marker-position prompt-start-marker))))
          (delete-region replace-from (point-max))
          (goto-char replace-from)
          (insert redirect-text))
        (when active-fsm
          (when (fboundp 'gptel-fsm-prompt)
            (with-no-warnings (setf (gptel-fsm-prompt active-fsm) redirect-text)))
          (macher-agent--set-fsm-info active-fsm
                                      (plist-put (gptel-fsm-info active-fsm) :prompt redirect-text))))

      (let* ((cur-info (and active-fsm (gptel-fsm-info active-fsm)))
             (wire-ok (and active-fsm
                           (not (eq temp-buf orig-buf))
                           (macher-agent--wire-enabled-p orig-buf cur-info)
                           (eq (buffer-local-value 'major-mode temp-buf)
                               (buffer-local-value 'major-mode orig-buf))
                           (not (with-current-buffer orig-buf (use-region-p)))))
             (_ (when wire-ok
                  (let ((eff-backend (plist-get payload :backend))
                        (prev-wire-backend (buffer-local-value 'macher-agent--wire-backend orig-buf)))
                    (when (and prev-wire-backend
                               (not (eq eff-backend prev-wire-backend)))
                      (macher-agent-clear-wire-buffer orig-buf))
                    (when (or (not (plist-get cur-info :dry-run))
                              (eq eff-backend (buffer-local-value 'gptel-backend orig-buf)))
                      (with-current-buffer orig-buf
                        (setq-local macher-agent--wire-backend eff-backend))))))
             (pos-m (and (macher-agent--plist-p cur-info) (plist-get cur-info :position)))
             (send-end (if (and (markerp pos-m) (eq (marker-buffer pos-m) orig-buf))
                           (marker-position pos-m)
                         (with-current-buffer orig-buf (point-max))))
             (orig-prompt-beg (when wire-ok
                                (with-current-buffer orig-buf
                                  (macher-agent--prompt-start-pos send-end))))
             (has-wire-spans (and wire-ok (macher-agent--wire-has-spans-p orig-buf)))
             (has-untracked-pre
              (and wire-ok
                   (not has-wire-spans)
                   (not (plist-get cur-info :dry-run))
                   (with-current-buffer orig-buf
                     (and (> orig-prompt-beg (point-min))
                          (not (string-blank-p
                                (buffer-substring-no-properties
                                 (point-min) orig-prompt-beg))))))))
        (when wire-ok
          (when (null gptel-include-reasoning)
            (setq-local gptel-include-reasoning t))
          (let* ((ps-pos (marker-position prompt-start-marker))
                 (clean-prompt-txt (buffer-substring-no-properties ps-pos (point-max)))
                 (upd-info (copy-sequence (gptel-fsm-info active-fsm))))
            (setq upd-info (plist-put upd-info :macher-agent-prompt-bounds
                                      (cons orig-prompt-beg send-end)))
            (setq upd-info (plist-put upd-info :macher-agent-clean-prompt-text
                                      clean-prompt-txt))
            (cond
             ((or has-wire-spans has-untracked-pre)
              (let ((wire-prefix
                     (macher-agent--reconcile-wire-history
                      orig-buf
                      orig-prompt-beg
                      (plist-get payload :backend)
                      (macher-agent-transmission-state-disable-local-tools state)
                      (with-current-buffer orig-buf
                        (bound-and-true-p gptel--num-messages-to-send))
                      (macher-agent--get-max-context-chars
                       orig-buf (plist-get payload :model)))))
                (when (> ps-pos (point-min))
                  (delete-region (point-min) ps-pos))
                (when wire-prefix
                  (when (consp gptel-system-prompt)
                    (setq-local gptel-system-prompt
                                (car (gptel--parse-directive gptel-system-prompt 'raw))))
                  (when (and (bound-and-true-p gptel-context)
                             (bound-and-true-p gptel-use-context)
                             (fboundp 'gptel-context--collect-media))
                    (let ((prev-mh (buffer-local-value 'macher-agent--wire-last-media-hash orig-buf))
                          (cur-mh (when-let* ((ml (ignore-errors (gptel-context--collect-media))))
                                    (sxhash-equal ml))))
                      (when (and prev-mh cur-mh (eql prev-mh cur-mh))
                        (setq-local gptel-context nil)))))
                (setq upd-info (plist-put upd-info :macher-agent-wire-prefix wire-prefix))))
             ((> ps-pos (point-min))
              (save-restriction
                (narrow-to-region (point-min) ps-pos)
                (macher-agent--extract-inline-skills (point-min) orig-buf))))
            (macher-agent--set-fsm-info active-fsm upd-info)
            (setf (gptel-fsm-info active-fsm) upd-info))))
      (set-marker prompt-start-marker nil)

      (when (and (macher-agent-transmission-state-disable-local-tools state)
                 (not (eq temp-buf orig-buf)))
        (macher-agent--flatten-wire-tool-regions))

      (unless (eq temp-buf orig-buf)
        (setq-local gptel--known-presets nil)))

    (when-let* ((fn async-fn)
                ((functionp fn)))
      (funcall fn))
    (when active-fsm
      (macher-agent--apply-wire-prefix-to-fsm active-fsm))))

;;; Transmission Execution

(defun macher-agent--submission-task-id (submission)
  "Extract `:task-id' from staged or delivered SUBMISSION record, or nil."
  (cond
   ((macher-agent--plist-p submission)
    (or (plist-get submission :task-id)
        (when-let* ((payload (plist-get submission :a2a-payload))
                    ((macher-agent-transit-payload-p payload)))
          (macher-agent-transit-payload-task-id payload))))
   ((stringp submission) submission)
   ((and (symbolp submission) (not (memq submission '(nil t))))
    submission)
   (t nil)))

(defun macher-agent--submission-matches-expected-p (submission expected-task-id)
  "Return non-nil when SUBMISSION belongs to EXPECTED-TASK-ID."
  (when submission
    (let ((sub-id (macher-agent--submission-task-id submission)))
      (or (null expected-task-id)
          (null sub-id)
          (equal sub-id expected-task-id)))))

(defun macher-agent--extract-turn-response-text (&optional beg end)
  "Extract assistant response text between BEG and END, skipping ignored blocks."
  (let* ((has-bounds (and (integer-or-marker-p beg)
                          (integer-or-marker-p end)
                          (< beg end)))
         (b (if has-bounds (max (point-min) (min (point-max) beg)) (point-min)))
         (e (if has-bounds (max (point-min) (min (point-max) end)) (point-max)))
         (response-parts nil)
         (non-ignored-parts nil)
         (pos b))
    (while (< pos e)
      (let* ((prop (get-char-property pos 'gptel))
             (next (min e (next-single-property-change pos 'gptel nil e)))
             (chunk (buffer-substring-no-properties pos next)))
        (when (eq prop 'response)
          (push chunk response-parts))
        (unless (or (eq prop 'ignore)
                    (and (consp prop) (eq (car prop) 'reasoning)))
          (push chunk non-ignored-parts))
        (setq pos next)))
    (string-trim
     (if response-parts
         (apply #'concat (nreverse response-parts))
       (apply #'concat (nreverse non-ignored-parts))))))

(defun macher-agent--make-transmit-response-hook (on-error &optional on-success expected-task-id)
  "Build response hook invoking ON-ERROR or ON-SUCCESS on turn completion.
Skips ON-SUCCESS while a routing frame awaits `submit_task_result'.
When EXPECTED-TASK-ID is non-nil or routing is active at hook creation,
scope submission and error checks to that task ID."
  (cl-check-type on-error function)
  (let ((hook-fn nil)
        (recovery-attempted nil)
        (expected-task-id
         (or expected-task-id
             (plist-get (car-safe (bound-and-true-p macher-agent--routing-stack)) :task-id)
             (bound-and-true-p macher-agent--current-task-id))))
    (setq
     hook-fn
     (lambda (beg end)
       (let* ((res (string-trim (buffer-substring-no-properties (point-min) (point-max))))
              (fsm (macher-agent-get-active-fsm))
              (info (and fsm (fboundp 'gptel-fsm-info) (ignore-errors (gptel-fsm-info fsm))))
              (err-data (and info (plist-get info :error)))
              (tool-calls (and info (plist-get info :tool-use)))
              (disable-local-tools
               (or (and info (plist-get info :disable-local-tools))
                   (bound-and-true-p macher-agent--disable-local-tools)))
              (routing-stack (bound-and-true-p macher-agent--routing-stack))
              (top-task-id (or (plist-get (car-safe routing-stack) :task-id)
                               (bound-and-true-p macher-agent--current-task-id)))
              (has-routing (or routing-stack
                               (bound-and-true-p macher-agent--current-task-id)))
              (staged (bound-and-true-p macher-agent--staged-submission))
              (delivered (bound-and-true-p macher-agent--turn-submission-delivered))
              (finished (bound-and-true-p macher-agent-task-finished))
              (staged-matches
               (macher-agent--submission-matches-expected-p staged expected-task-id))
              (delivered-matches
               (macher-agent--submission-matches-expected-p delivered expected-task-id))
              (other-submission-active
               (or (and staged (not staged-matches))
                   (and delivered (not delivered-matches))))
              (other-task-on-top
               (and expected-task-id
                    top-task-id
                    (not (equal expected-task-id top-task-id))))
              (expected-frame-gone
               (and expected-task-id
                    (not (equal expected-task-id
                                (bound-and-true-p macher-agent--current-task-id)))
                    (not (cl-some (lambda (frame)
                                    (equal (plist-get frame :task-id) expected-task-id))
                                  routing-stack)))))
         (cond
          ((and err-data (not other-task-on-top))
           (setq-local macher-agent--submit-recovery-pending nil)
           (setq-local macher-agent--staged-submission nil)
           (remove-hook 'gptel-post-response-functions hook-fn t)
           (let ((err-msg (if (stringp err-data) err-data
                            (or (plist-get err-data :message)
                                (format "%s" err-data)))))
             (funcall on-error err-msg)))
          (expected-frame-gone
           (unless other-task-on-top
             (setq-local macher-agent--submit-recovery-pending nil))
           (remove-hook 'gptel-post-response-functions hook-fn t))
          ((or staged-matches
               delivered-matches
               (and finished (not other-task-on-top) (not other-submission-active)))
           (setq-local macher-agent--submit-recovery-pending nil)
           (remove-hook 'gptel-post-response-functions hook-fn t))
          ((or other-submission-active other-task-on-top)
           nil)
          ((string-empty-p res)
           (setq-local macher-agent--submit-recovery-pending nil)
           (remove-hook 'gptel-post-response-functions hook-fn t)
           (funcall on-error "Buffer stopped silently or returned empty."))
          ((and (not has-routing)
                (not tool-calls))
           (setq-local macher-agent--submit-recovery-pending nil)
           (setq-local macher-agent-task-finished t)
           (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context))
                       ((macher-agent-context-p ctx)))
             (setf (macher-agent-session-state (macher-agent-context-session ctx)) :finished))
           (remove-hook 'gptel-post-response-functions hook-fn t)
           (when (functionp on-success)
             (funcall on-success res)))
          ((and has-routing
                (not tool-calls)
                (not finished))
           (if disable-local-tools
               (let ((turn-res (macher-agent--extract-turn-response-text beg end)))
                 (setq-local macher-agent--submit-recovery-pending nil)
                 (remove-hook 'gptel-post-response-functions hook-fn t)
                 (if (string-empty-p turn-res)
                     (funcall on-error "Buffer stopped silently or returned empty.")
                   (when (functionp macher-agent-direct-task-submit-function)
                     (funcall macher-agent-direct-task-submit-function
                              (current-buffer)
                              turn-res
                              expected-task-id
                              (bound-and-true-p macher-agent--persistent-context)))))
             (if (not recovery-attempted)
                 (progn
                   (setq recovery-attempted t)
                   (setq-local macher-agent--submit-recovery-pending t)
                   (save-excursion
                     (goto-char (point-max))
                     (let* ((raw-prefix (or (and (boundp 'gptel-prompt-prefix-alist)
                                                 (alist-get major-mode gptel-prompt-prefix-alist))
                                            (when (fboundp 'gptel-prompt-prefix-string)
                                              (ignore-errors (gptel-prompt-prefix-string)))
                                            ""))
                            (prefix (if (stringp raw-prefix) raw-prefix ""))
                            (formatted-prefix
                             (cond
                              ((string-empty-p prefix) "")
                              ((string-match-p "[ \t\r\n]\\'" prefix) prefix)
                              (t (concat prefix " "))))
                            (sep (cond
                                  ((bobp) "")
                                  ((looking-back "\n\n" (max (point-min) (- (point) 2))) "")
                                  ((looking-back "\n" (max (point-min) (- (point) 1))) "\n")
                                  (t "\n\n")))
                            (msg-body "CRITICAL: You must submit your final answer using the `submit_task_result` tool. Do not reply with plain text. Call `submit_task_result` now.")
                            (msg-text (concat sep formatted-prefix msg-body))
                            (ins-beg (point)))
                       (insert msg-text)
                       (remove-text-properties ins-beg (point) '(gptel nil front-sticky nil))))
                   (goto-char (point-max))
                   (condition-case send-err
                       (gptel-send)
                     (error
                      (setq-local macher-agent--submit-recovery-pending nil)
                      (remove-hook 'gptel-post-response-functions hook-fn t)
                      (funcall on-error (error-message-string send-err)))))
               (setq-local macher-agent--submit-recovery-pending nil)
               (remove-hook 'gptel-post-response-functions hook-fn t)
               (funcall on-error "Agent halted without invoking submit_task_result."))))))))
    hook-fn))

(defun macher-agent-gptel-transmit (task-context callbacks)
  "Transmit request for TASK-CONTEXT using CALLBACKS plist."
  (let* ((target-buffer (macher-agent-task-context-target-buffer task-context))
         (sys-msg (macher-agent-task-context-system-message task-context))
         (ctx-task-id (macher-agent-task-context-task-id task-context))
         (on-error (plist-get callbacks :on-error))
         (on-success (plist-get callbacks :on-success)))
    (with-current-buffer target-buffer
      (let ((expected-task-id
             (or ctx-task-id
                 (plist-get (car-safe (bound-and-true-p macher-agent--routing-stack)) :task-id)
                 (bound-and-true-p macher-agent--current-task-id))))
        (setq-local macher-agent--turn-submission-delivered nil)
        (when sys-msg
          (setq-local gptel-system-prompt sys-msg))
        (when (functionp on-error)
          (add-hook 'gptel-post-response-functions
                    (macher-agent--make-transmit-response-hook
                     on-error on-success expected-task-id)
                    nil t))
        (goto-char (point-max))
        (gptel-send)))))

;;; Media Injection

(defvar macher-agent--in-media-injection nil
  "Track whether media injection is currently in progress.")

(defun macher-agent-gptel--fsm-target-buffer (fsm)
  "Extract target buffer from FSM safely."
  (when fsm
    (let* ((info (ignore-errors (macher-agent--extract-fsm-info fsm))))
      (when (macher-agent--plist-p info)
        (or (plist-get info :origin-buffer)
            (plist-get info :buffer))))))

(defun macher-agent-gptel-context-from-fsm (fsm &optional target-buf)
  "Extract agent context from FSM, falling back to TARGET-BUF."
  (if (macher-agent-context-p fsm)
      fsm
    (or (macher-agent-resolve-context fsm)
        (macher-agent-resolve-context (or target-buf (macher-agent-gptel--fsm-target-buffer fsm) (current-buffer))))))

(defun macher-agent--perform-pending-media-injection (fsm)
  "Inject base64 media directly into FSM payload."
  (let* ((info (ignore-errors (gptel-fsm-info fsm)))
         (ctx (when (macher-agent--plist-p info)
                (or (plist-get info :macher-agent-context)
                    (plist-get info :context))))
         (media (when ctx
                  (if (macher-agent-context-p ctx)
                      (or (macher-agent-context-media-queue ctx)
                          (plist-get (macher-agent-context-plugins ctx) :pending-media))
                    (when (macher-agent--plist-p ctx)
                      (or (plist-get ctx :media-queue)
                          (plist-get ctx :pending-media)))))))
    (when media
      (unless (or (stringp media) (consp media))
        (error "SECURITY ERROR: Media injection strictly requires a base64 encoded string or media descriptor"))
      (let* ((backend (plist-get info :backend))
             (data (plist-get info :data))
             (msg-text "Tool execution complete. Here is the requested visual data:")
             (parsed-list (when (fboundp 'gptel--parse-list)
                            (gptel--parse-list backend (list msg-text))))
             (msg-plist (if parsed-list
                            (car parsed-list)
                          (list :role "user" :content msg-text)))
             (prompts (list msg-plist))
             (media-spec (if (consp media) media (list media :mime "image/png")))
             (gptel-context (list media-spec)))
        (when (and backend (fboundp 'gptel--inject-media))
          (cl-letf (((symbol-function 'gptel--base64-encode)
                     (lambda (file)
                       (if (and (stringp file) (or (> (length file) 1000) (not (file-exists-p file))))
                           file
                         (with-temp-buffer
                           (insert-file-contents-literally file)
                           (base64-encode-region (point-min) (point-max) :no-line-break)
                           (buffer-string))))))
            (gptel--inject-media backend prompts)))
        (when (and backend data (fboundp 'gptel--inject-prompt))
          (gptel--inject-prompt backend data (car prompts))))
      (when (macher-agent-context-p ctx)
        (setf (macher-agent-context-media-queue ctx) nil)
        (when-let* ((plugins (macher-agent-context-plugins ctx)))
          (setf (macher-agent-context-plugins ctx)
                (plist-put (copy-sequence plugins) :pending-media nil)))))))

(defun macher-agent--inject-media-fsm-logic (fsm)
  "Inject pending tool media into FSM payload when entering WAIT state."
  (unless macher-agent--in-media-injection
    (macher-agent--perform-pending-media-injection fsm)))

;;; Tool Scope Enforcement and Buffer Setup

(defun macher-agent--enforce-tool-scope (tool &optional fsm &rest _args)
  "Enforce that TOOL is within the authorised scope of FSM."
  (let* ((canonical-name (macher-agent-canonical-tool-name tool))
         (fsm-obj (macher-agent-get-active-fsm fsm))
         (info (when fsm-obj (ignore-errors (gptel-fsm-info fsm-obj))))
         (fsm-tools (or (when info (plist-get info :tools))
                        (bound-and-true-p gptel-tools)))
         (authorised-names (mapcar #'macher-agent-canonical-tool-name fsm-tools)))
    (unless (and canonical-name (member canonical-name authorised-names))
      (list
       :block (format "ERROR: Tool '%s' is not accessible in this context or is no longer available. Please select another tool or approach."
                      (or canonical-name tool))))))

(defun macher-agent--log-gptel-pre-tool (tool &optional fsm &rest args)
  "Log pre-tool call intent for TOOL, FSM, and ARGS to audit log."
  (let ((context (or (when fsm (macher-agent-gptel-context-from-fsm fsm))
                     (macher-agent-resolve-context)))
        (tool-name (macher-agent-canonical-tool-name tool))
        (tool-args (if (and (macher-agent--plist-p tool) (plist-member tool :args))
                       (plist-get tool :args)
                     args)))
    (when context
      (macher-agent-log-tool-intent context "gptel-tool" tool-name tool-args))))

(defun macher-agent--missing-resolvable-tool-p (saved-names)
  "Return non-nil when any SAVED-NAMES tool is resolvable but not active.
Checks whether a resolvable tool is absent from `gptel-tools'."
  (when (and (listp saved-names) saved-names (fboundp 'gptel-get-tool))
    (let ((active-names
           (delq nil (mapcar #'macher-agent-canonical-tool-name
                             (bound-and-true-p gptel-tools)))))
      (cl-some
       (lambda (tname)
         (when-let* ((resolved (ignore-errors (gptel-get-tool tname))))
           (let ((tools (if (and (listp resolved)
                                 (not (macher-agent--plist-p resolved)))
                            resolved
                          (list resolved))))
             (cl-some
              (lambda (tl)
                (when-let* ((cname (macher-agent-canonical-tool-name tl)))
                  (not (member cname active-names))))
              tools))))
       saved-names))))

(defun macher-agent--needs-preset-reconcile-p ()
  "Return non-nil when file-backed buffer metadata needs re-restoration."
  (when (buffer-file-name)
    (let* ((flv (bound-and-true-p file-local-variables-alist))
           (has-file-meta
            (or (assq 'gptel--preset flv)
                (assq 'gptel--bounds flv)
                (assq 'gptel--tool-names flv)
                (assq 'gptel--backend-name flv)
                (and (derived-mode-p 'org-mode)
                     (fboundp 'org-entry-get)
                     (or (org-entry-get (point-min) "GPTEL_PRESET" 'selective)
                         (org-entry-get (point-min) "GPTEL_BOUNDS" 'selective)
                         (org-entry-get (point-min) "GPTEL_TOOLS" 'selective)))))
           (preset-sym (bound-and-true-p gptel--preset))
           (full-spec (when preset-sym
                        (macher-agent--lookup-skill-spec preset-sym nil (current-buffer))))
           (preset-spec (and preset-sym
                             (not (plist-get full-spec :is-command))
                             (fboundp 'gptel-get-preset)
                             (ignore-errors (gptel-get-preset preset-sym))))
           (parsed-sys (car-safe (gptel--parse-directive gptel-system-prompt))))
      (and has-file-meta
           (or (and preset-spec
                    (not (assq 'gptel-system-prompt flv))
                    (fboundp 'gptel--preset-mismatch-value)
                    (gptel--preset-mismatch-value preset-spec :system parsed-sys))
               (and (derived-mode-p 'org-mode)
                    (null preset-sym)
                    (fboundp 'org-entry-get)
                    (when-let* ((raw-org-preset
                                 (org-entry-get (point-min) "GPTEL_PRESET" 'selective))
                                (trimmed (string-trim raw-org-preset))
                                ((not (string-empty-p trimmed)))
                                (org-preset-sym (macher-normalise-preset-name trimmed))
                                (org-full-spec (macher-agent--lookup-skill-spec
                                                org-preset-sym nil (current-buffer))))
                      (and (not (plist-get org-full-spec :is-command))
                           (fboundp 'gptel-get-preset)
                           (ignore-errors (gptel-get-preset org-preset-sym)))))
               (and (derived-mode-p 'org-mode)
                    (fboundp 'org-entry-get)
                    (when-let* ((org-tools (org-entry-get (point-min) "GPTEL_TOOLS" 'selective)))
                      (macher-agent--missing-resolvable-tool-p (split-string org-tools))))
               (and (bound-and-true-p gptel--tool-names)
                    (macher-agent--missing-resolvable-tool-p gptel--tool-names))
               (cl-some (lambda (cell)
                          (not (equal (and (boundp (car cell)) (symbol-value (car cell)))
                                      (cdr cell))))
                        (macher-agent--flv-overrides)))))))

(defun macher-agent--install-gptel-save-hooks ()
  "Register buffer-local save and prompt-transform hooks for Macher Agent."
  (add-hook 'gptel-save-state-hook #'macher-agent--sync-preset-before-save nil t)
  (add-hook 'before-save-hook #'macher-agent--persist-missing-preset-tools 90 t)
  (remove-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer t)
  (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t))

(defun macher-agent--teardown-gptel-buffer ()
  "Remove buffer-local `gptel' hooks installed by Macher Agent."
  (remove-hook 'gptel-save-state-hook #'macher-agent--sync-preset-before-save t)
  (remove-hook 'before-save-hook #'macher-agent--persist-missing-preset-tools t)
  (remove-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer t)
  (dolist (transformer macher-agent-prompt-transformers)
    (remove-hook 'gptel-prompt-transform-functions transformer t))
  (remove-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope t))

(defun macher-agent-setup-gptel-buffer ()
  "Set up a gptel buffer with macher-agent capabilities."
  (setq-local macher-agent--active-fsm nil)
  (macher-agent--reconcile-restored-buffer)
  (dolist (transformer (reverse macher-agent-prompt-transformers))
    (remove-hook 'gptel-prompt-transform-functions transformer t)
    (add-hook 'gptel-prompt-transform-functions transformer nil t))
  (macher-agent--install-gptel-save-hooks)
  (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t))

(defun macher-agent--init-workspace-state (workspace-root)
  "Initialise the workspace state and active context for WORKSPACE-ROOT."
  (unless (bound-and-true-p macher-agent--is-workspace)
    (setq-local macher-agent--is-workspace t)
    (setq-local macher-agent--active-fsm nil)
    (let* ((expanded (file-truename (expand-file-name workspace-root)))
           (existing (bound-and-true-p macher-agent--persistent-context))
           (workspace (or (and existing (macher-agent-context-workspace existing))
                          (make-macher-agent-workspace :project-root expanded)))
           (canonical-context
            (or existing
                (macher-agent--make-context
                 :project-root expanded
                 :origin-buffer (current-buffer)
                 :tools (make-hash-table :test 'equal)
                 :plugins (list :workspace workspace))))
           (buffer-context (when canonical-context
                             (macher-agent--clone-context canonical-context))))
      (when buffer-context
        (setf (macher-agent-context-owner-buffer buffer-context) (current-buffer))
        (setf (macher-agent-context-origin-buffer buffer-context) (current-buffer))
        (macher-agent--register-active-workspace-root expanded buffer-context)
        (macher-agent--inject-context-state buffer-context))
      (macher-agent--install-gptel-save-hooks)
      (when buffer-context
        (macher-agent-refresh-skills-and-tools buffer-context)))))

(defun macher-agent-gptel-mode-setup ()
  "Initialise agent defaults for gptel buffers inside a workspace."
  (let ((is-workspace
         (and default-directory
              (or (bound-and-true-p macher-agent--is-workspace)
                  (and (fboundp 'project-current)
                       (project-current nil default-directory))
                  (and (fboundp 'vc-root-dir)
                       (let ((default-directory default-directory))
                         (vc-root-dir)))))))
    (unless is-workspace
      (if (not (and default-directory (file-directory-p default-directory)))
          (progn
            (macher-agent--teardown-gptel-buffer)
            (user-error "Macher-Agent: Directory '%s' is not in a recognised project workspace" default-directory))
        (condition-case err
            (setq is-workspace (macher-agent--ensure-git-root default-directory))
          (error
           (macher-agent--teardown-gptel-buffer)
           (user-error "%s" (error-message-string err))))))
    (make-local-variable 'gptel--preset)
    (make-local-variable 'gptel-tools)
    (make-local-variable 'gptel-model)
    (make-local-variable 'gptel-backend)
    (make-local-variable 'gptel-system-prompt)
    (make-local-variable 'gptel-temperature)
    (make-local-variable 'gptel-max-tokens)
    (make-local-variable 'gptel--tool-names)
    (make-local-variable 'gptel--backend-name)
    (setq-local gptel--set-buffer-locally t)
    (macher-agent--init-workspace-state
     (file-name-as-directory (macher-agent-root default-directory)))
    t))

(defun macher-agent-branch-chat (new-name)
  "Clone current chat buffer into NEW-NAME, inheriting agent state."
  (interactive "sNew branch name: ")
  (let* ((parent-buf (current-buffer))
         (parent-mode major-mode)
         (content (buffer-string))
         (active-backend gptel-backend)
         (active-model gptel-model)
         (active-sys gptel-system-prompt)
         (active-preset (bound-and-true-p gptel--preset))
         (active-skill (bound-and-true-p macher-agent--active-skill-sym))
         (parent-ctx (macher-agent-resolve-context parent-buf)))
    (with-current-buffer (generate-new-buffer new-name)
      (funcall parent-mode)
      (gptel-mode)
      (insert content)
      (when active-backend (setq-local gptel-backend active-backend))
      (when active-model (setq-local gptel-model active-model))
      (when active-sys (setq-local gptel-system-prompt active-sys))
      (when active-preset (setq-local gptel--preset active-preset))
      (when active-skill (setq-local macher-agent--active-skill-sym active-skill))
      (when parent-ctx
        (let ((cloned (macher-agent--clone-context parent-ctx)))
          (setf (macher-agent-context-owner-buffer cloned) (current-buffer))
          (setf (macher-agent-context-parent-buffer cloned) parent-buf)
          (setq-local macher-agent--persistent-context cloned)))
      (setq-local macher-agent-presets (with-current-buffer parent-buf macher-agent-presets))
      (setq-local gptel-tools (with-current-buffer parent-buf gptel-tools))
      (when (local-variable-p 'gptel-include-reasoning parent-buf)
        (setq-local gptel-include-reasoning
                    (buffer-local-value 'gptel-include-reasoning parent-buf)))
      (when (local-variable-p 'gptel-include-tool-results parent-buf)
        (setq-local gptel-include-tool-results
                    (buffer-local-value 'gptel-include-tool-results parent-buf)))
      (when (or (buffer-local-value 'macher-agent--is-workspace parent-buf)
                (and (boundp 'macher-agent-mode)
                     (buffer-local-value 'macher-agent-mode parent-buf)))
        (setq-local macher-agent--is-workspace t)
        (macher-agent-setup-gptel-buffer))
      (macher-agent--clone-wire-buffer parent-buf (current-buffer))
      (switch-to-buffer (current-buffer)))))

(defun macher-agent-clear-context ()
  "Clear the persistent context for the current sub-agent buffer."
  (interactive)
  (if (not (bound-and-true-p macher-agent--persistent-context))
      (message "Macher-Agent: No persistent context to clear in buffer '%s'." (buffer-name))
    (let* ((raw-ws (macher-agent-context-workspace macher-agent--persistent-context))
           (ws (if (stringp raw-ws) (cons 'agent raw-ws) raw-ws))
           (root (or (macher-agent-context-project-root macher-agent--persistent-context)
                     default-directory))
           (fresh-ctx (when ws
                        (macher-agent--make-context
                         :project-root (expand-file-name root)
                         :origin-buffer (current-buffer)
                         :tools (make-hash-table :test 'equal)
                         :plugins (list :workspace ws)))))
      (when fresh-ctx
        (setf (macher-agent-context-owner-buffer fresh-ctx) (current-buffer)))
      (setq-local macher-agent--persistent-context fresh-ctx)
      (when (and (bound-and-true-p macher-agent--active-fsm)
                 (fboundp 'gptel-fsm-info))
        (let ((inf (gptel-fsm-info macher-agent--active-fsm)))
          (when (macher-agent--plist-p inf)
            (setq inf (plist-put inf :macher-agent-context fresh-ctx))
            (macher-agent--set-fsm-info macher-agent--active-fsm inf))))
      (message "Macher-Agent: VFS context successfully cleared. Agent reset to physical baseline."))))

(defun macher-agent-send (&rest args)
  "Trigger transmission in current buffer using `gptel-send' with ARGS."
  (apply #'gptel-send args))

(defun macher-agent-gptel--trigger-flush (fsm &rest _args)
  "Broadcast task completion for FSM using the associated agent context."
  (when (and fsm (fboundp 'gptel-fsm-state) (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT)))
    (when-let* ((info (ignore-errors (gptel-fsm-info fsm)))
                (target-buf (plist-get info :buffer)))
      (let* ((raw-ctx (or (plist-get info :macher-agent-context)
                          (macher-agent-resolve-context target-buf)))
             (agent-ctx (when (macher-agent-valid-context-p raw-ctx) raw-ctx)))
        (if (buffer-live-p target-buf)
            (with-current-buffer target-buf
              (when (eq (gptel-fsm-state fsm) 'DONE)
                (let ((rend-m (plist-get info :macher-agent-response-end-marker)))
                  (when (and (markerp rend-m)
                             (eq (marker-buffer rend-m) target-buf)
                             (< (marker-position rend-m) (point-max)))
                    (set-marker-insertion-type rend-m t))))
              (when (memq (gptel-fsm-state fsm) '(ERRS ABRT))
                (setq-local macher-agent--staged-submission nil))
              (when (eq macher-agent--active-fsm fsm)
                (setq-local macher-agent--active-fsm nil))
              (run-hook-with-args 'macher-agent-buffer-flush-hook target-buf)
              (when agent-ctx
                (macher-agent-run-task-flush-hook agent-ctx)))
          (when agent-ctx
            (macher-agent-run-task-flush-hook agent-ctx)))))))

(defun macher-agent-get-active-fsm (&optional current-fsm)
  "Correlate UI operations using validated CURRENT-FSM."
  (or (and current-fsm
           (not (macher-agent-transit-payload-p current-fsm))
           (not (macher-agent-context-p current-fsm))
           (not (bufferp current-fsm))
           current-fsm)
      (bound-and-true-p macher-agent--active-fsm)
      (bound-and-true-p gptel--fsm)))

(setq macher-agent-active-fsm-function #'macher-agent-get-active-fsm)
(setq macher-agent-fsm-info-function #'gptel-fsm-info)

(defun macher-agent--extract-fsm-info (fsm)
  "Extract info plist from FSM struct."
  (cl-check-type fsm gptel-fsm)
  (gptel-fsm-info fsm))

(defun macher-agent--set-fsm-info (fsm info)
  "Set INFO plist on FSM struct."
  (when fsm
    (setf (gptel-fsm-info fsm) info)
    info))

(defun macher-agent--set-fsm-handlers (fsm handlers)
  "Set HANDLERS alist on FSM struct."
  (when fsm
    (setf (gptel-fsm-handlers fsm) handlers)
    handlers))

(defun macher-agent-bridge-abort (buf)
  "Abort operations exclusively in live BUF object."
  (cl-check-type buf buffer)
  (when (and (buffer-live-p buf) (fboundp 'gptel-abort))
    (with-current-buffer buf
      (condition-case nil
          (gptel-abort buf)
        (wrong-number-of-arguments
         (funcall (symbol-function 'gptel-abort)))))))

(provide 'macher-agent-gptel)
;;; macher-agent-gptel.el ends here
