;;; macher-agent-wire.el --- Lossless shadow wire buffer for Macher Agent -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Version: 0.8.3.66
;; Package-Requires: ((emacs "30.1") (gptel "0.9.9.6") (macher-agent "0.8.3.66"))
;; Keywords: convenience, gptel, llm
;; URL: https://github.com/elij/macher-agent
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Maintains a lossless hidden shadow wire buffer for multi-turn replay in
;; Macher Agent.  Captures provider wire entries (`thoughtSignature',
;; reasoning blocks, tool calls, and full tool results) independently of
;; visible chat buffer display settings and reconciles user edits, turn
;; deletions, branch cloning, and quantised context compaction epochs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'gptel)
(require 'macher-agent)

(declare-function gptel--create-prompt-buffer "gptel-request" (&optional prompt-end))
(declare-function gptel--parse-buffer "gptel-request" (backend max-entries))
(declare-function gptel--parse-list "gptel-request" (backend prompt-list))
(declare-function gptel--parse-list-and-insert "gptel-request" (prompts))
(declare-function gptel--parse-directive "gptel-request" (directive &optional raw))
(declare-function gptel--inject-prompt "gptel-request" (backend data new-prompt &optional position))
(declare-function gptel--handle-wait "gptel-request" (fsm))
(declare-function gptel--json-encode "gptel-request" (object))
(declare-function gptel--insert-response "gptel" (response info &optional raw))
(declare-function gptel-curl--stream-insert-response "gptel" (response info &optional raw))
(declare-function gptel-prompt-prefix-string "gptel" ())
(declare-function gptel-response-prefix-string "gptel" ())
(declare-function gptel-context--collect-media "gptel-context" ())

(defvar gptel-backend)
(defvar gptel-cache)
(defvar gptel-context)
(defvar gptel-include-reasoning)
(defvar gptel--num-messages-to-send)
(defvar gptel-prompt-transform-functions)
(defvar gptel-system-prompt)
(defvar gptel-track-response)
(defvar gptel-use-context)
(defvar gptel-use-curl)

(defgroup macher-agent-wire nil
  "Lossless shadow wire buffer integration for Macher Agent."
  :group 'macher-agent
  :prefix "macher-agent-wire-")

(defcustom macher-agent-wire-buffer-mode t
  "When non-nil, maintain a lossless shadow wire buffer for multi-turn replay.
Captures provider wire entries (`thoughtSignature', reasoning blocks,
tool calls, and full tool results) in a hidden shadow buffer and replays
them across turns independently of user-buffer display settings such as
`gptel-include-reasoning' and `gptel-include-tool-results'.  When nil,
the shadow wire buffer is disabled and `gptel' parses the visible chat
buffer directly."
  :type 'boolean
  :group 'macher-agent-wire)

(defvar-local macher-agent-wire-buffer nil
  "Hidden shadow buffer storing lossless wire entries and verbose transcripts.")
(put 'macher-agent-wire-buffer 'permanent-local t)

(defvar-local macher-agent-wire-origin-buffer nil
  "User-facing chat buffer associated with the current shadow wire buffer.")
(put 'macher-agent-wire-origin-buffer 'permanent-local t)

(defvar-local macher-agent-wire-turn-counter 0
  "Monotonic turn counter assigning `macher-agent-turn-id' values.")
(put 'macher-agent-wire-turn-counter 'permanent-local t)

(defvar-local macher-agent-wire-horizon-turn-id nil
  "Pinned truncation epoch horizon (`turn-id' integer or `(gap . span-id)').")
(put 'macher-agent-wire-horizon-turn-id 'permanent-local t)

(defvar-local macher-agent-wire-horizon-lines-deleted nil
  "Pinned truncated line count for the active quantised compaction epoch.")
(put 'macher-agent-wire-horizon-lines-deleted 'permanent-local t)

(defvar-local macher-agent-wire-dedup-turn-id nil
  "Upper `turn-id' boundary pinned for epoch wire tool-result deduplication.")
(put 'macher-agent-wire-dedup-turn-id 'permanent-local t)

(defvar-local macher-agent-wire-backend nil
  "Backend instance associated with committed spans in the shadow wire buffer.")
(put 'macher-agent-wire-backend 'permanent-local t)

(defvar-local macher-agent-wire--span-counter 0
  "Monotonic span counter inside a shadow wire buffer.")
(put 'macher-agent-wire--span-counter 'permanent-local t)

(defvar-local macher-agent-wire--last-media-hash nil
  "Hash of `gptel-context' media already injected into cached wire history.")
(put 'macher-agent-wire--last-media-hash 'permanent-local t)

;;; Shadow Buffer Lifecycle and Inspection

(defun macher-agent-wire--clear-buffer-markers (wbuf &optional beg end)
  "Detach markers stored on text properties in WBUF between BEG and END."
  (when (and (bufferp wbuf) (buffer-live-p wbuf))
    (with-current-buffer wbuf
      (let ((pos (or beg (point-min)))
            (limit (or end (point-max))))
        (while (< pos limit)
          (dolist (prop '(macher-agent-orig-beg
                          macher-agent-orig-end
                          macher-agent-prompt-beg
                          macher-agent-prompt-end))
            (let ((m (get-text-property pos prop)))
              (when (markerp m)
                (set-marker m nil))))
          (setq pos (next-property-change pos wbuf limit)))))))

(defun macher-agent-wire--cleanup-buffer ()
  "Detach markers and kill the shadow wire buffer owned by `current-buffer'."
  (let ((wbuf (bound-and-true-p macher-agent-wire-buffer)))
    (setq-local macher-agent-wire-buffer nil)
    (when (and (bufferp wbuf) (buffer-live-p wbuf))
      (macher-agent-wire--clear-buffer-markers wbuf)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer wbuf)))))

(defun macher-agent-wire-buffer-for (buf &optional create)
  "Return live shadow wire buffer for BUF, creating it when CREATE is non-nil."
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((existing (buffer-local-value 'macher-agent-wire-buffer buf)))
      (cond
       ((and (bufferp existing) (buffer-live-p existing))
        existing)
       (create
        (let ((wbuf (generate-new-buffer
                     (format " *macher-agent-wire:%s*" (buffer-name buf)))))
          (with-current-buffer wbuf
            (fundamental-mode)
            (buffer-disable-undo)
            (setq-local bidi-display-reordering nil)
            (setq-local bidi-paragraph-direction 'left-to-right)
            (setq-local font-lock-mode nil)
            (setq-local jit-lock-mode nil)
            (setq-local gptel-track-response t)
            (setq-local macher-agent-wire-origin-buffer buf))
          (with-current-buffer buf
            (setq-local macher-agent-wire-buffer wbuf)
            (add-hook 'kill-buffer-hook #'macher-agent-wire--cleanup-buffer nil t))
          wbuf))
       (t nil)))))

(defun macher-agent-wire-has-spans-p (buf)
  "Return non-nil when BUF owns a shadow wire buffer with committed spans."
  (when-let* ((wbuf (macher-agent-wire-buffer-for buf nil)))
    (with-current-buffer wbuf
      (and (> (buffer-size) 0)
           (not (null (text-property-not-all
                       (point-min) (point-max) 'macher-agent-turn-id nil)))))))

(defun macher-agent-wire-total-chars (buf)
  "Return total wire character count tracked in shadow buffer for BUF."
  (if-let* ((wbuf (macher-agent-wire-buffer-for buf nil)))
      (with-current-buffer wbuf
        (let ((pos (point-min))
              (total 0))
          (while (< pos (point-max))
            (let ((sid (get-text-property pos 'macher-agent-span-id))
                  (next (next-single-property-change
                         pos 'macher-agent-span-id wbuf (point-max)))
                  (chars (get-text-property pos 'macher-agent-wire-chars)))
              (when (and sid (integerp chars))
                (setq total (+ total chars)))
              (setq pos next)))
          (if (> total 0) total (buffer-size))))
    0))

(defun macher-agent-wire-history-chars (buf)
  "Return history character count for BUF across visible and wire buffers."
  (max (macher-agent-default-history-chars buf)
       (macher-agent-wire-total-chars buf)))

(defun macher-agent-wire-source-buffer (orig-buf)
  "Return populated shadow wire buffer for ORIG-BUF, or ORIG-BUF itself."
  (if (and (bufferp orig-buf) (buffer-live-p orig-buf)
           (macher-agent-wire-has-spans-p orig-buf))
      (or (macher-agent-wire-buffer-for orig-buf nil) orig-buf)
    orig-buf))

(defun macher-agent-wire-has-prior-response-p (buf)
  "Return non-nil when BUF has committed wire spans or prior responses."
  (when (and (bufferp buf) (buffer-live-p buf))
    (or (macher-agent-wire-has-spans-p buf)
        (with-current-buffer buf
          (not (null (text-property-any (point-min) (point-max) 'gptel 'response)))))))

(defun macher-agent-wire--enabled-p (buf &optional info)
  "Return non-nil when shadow wire caching is enabled for BUF and FSM INFO."
  (and macher-agent-wire-buffer-mode
       (bufferp buf)
       (buffer-live-p buf)
       (or (buffer-local-value 'macher-agent-is-workspace buf)
           (and (boundp 'macher-agent-mode)
                (buffer-local-value 'macher-agent-mode buf))
           (and (macher-agent-plist-p info)
                (macher-agent-valid-context-p
                 (or (plist-get info :macher-agent-context)
                     (plist-get info :context))))
           (macher-agent-valid-context-p
            (buffer-local-value 'macher-agent-persistent-context buf)))))

(defun macher-agent-wire-deep-copy-data (obj)
  "Return an immutable deep copy of wire structure OBJ without text properties."
  (cond
   ((stringp obj) (substring-no-properties obj))
   ((vectorp obj)
    (let* ((len (length obj))
           (vec (make-vector len nil)))
      (dotimes (i len)
        (aset vec i (macher-agent-wire-deep-copy-data (aref obj i))))
      vec))
   ((consp obj)
    (let ((head nil)
          (cur obj))
      (while (consp cur)
        (push (macher-agent-wire-deep-copy-data (car cur)) head)
        (setq cur (cdr cur)))
      (let ((tail (macher-agent-wire-deep-copy-data cur)))
        (dolist (elt head tail)
          (setq tail (cons elt tail))))))
   (t obj)))

(defun macher-agent-wire-estimate-chars (entries)
  "Estimate JSON character footprint of wire ENTRIES."
  (cond
   ((null entries) 0)
   ((fboundp 'gptel--json-encode)
    (condition-case nil
        (length (gptel--json-encode (vconcat entries)))
      (error (length (prin1-to-string entries)))))
   (t (length (prin1-to-string entries)))))

(defun macher-agent-wire--normalize-text (str &optional prompt-pfx response-pfx)
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

(defun macher-agent-wire-norm-hash (buf beg end)
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
        (sxhash-equal (macher-agent-wire--normalize-text raw ppfx rpfx))))))

(defun macher-agent-wire-strip-local-variables ()
  "Strip trailing HTML file-local variables block from current wire buffer."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n")
    (let ((strip-beg nil)
          (keep t))
      (while (and keep (> (point) (point-min)))
        (let ((lbeg (line-beginning-position)))
          (cond
           ((save-excursion
              (goto-char lbeg)
              (looking-at "[ \t]*<!--+[ \t]*End:[ \t]*--+>[ \t]*$"))
            (goto-char lbeg)
            (if (re-search-backward
                 "^[ \t]*<!--+[ \t]*Local Variables:[ \t]*--+>[ \t]*$" nil t)
                (let ((beg (match-beginning 0)))
                  (if (memq (car-safe (ensure-list (get-text-property beg 'gptel)))
                            '(response tool))
                      (setq keep nil)
                    (setq strip-beg beg)
                    (goto-char beg)
                    (skip-chars-backward " \t\r\n")))
              (setq keep nil)))
           ((save-excursion
              (goto-char lbeg)
              (looking-at "[ \t]*<!--+[ \t]*macher-agent-gptel-bounds:.*--+>[ \t]*$"))
            (if (memq (car-safe (ensure-list (get-text-property lbeg 'gptel)))
                      '(response tool))
                (setq keep nil)
              (setq strip-beg lbeg)
              (goto-char lbeg)
              (skip-chars-backward " \t\r\n")))
           (t (setq keep nil)))))
      (when strip-beg
        (delete-region strip-beg (point-max))))))

(defun macher-agent-wire-last-committed-pos (buf &optional before-pos)
  "Return largest committed `macher-agent-orig-end' in BUF up to BEFORE-POS."
  (when-let* ((wbuf (macher-agent-wire-buffer-for buf nil)))
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

;;; Shadow Transcript Appenders and Callback Wrapper

(defun macher-agent-wire--shadow-close-reasoning-fence (wbuf fsm)
  "Close an open reasoning fence in shadow buffer WBUF for FSM."
  (when (and fsm (bufferp wbuf) (buffer-live-p wbuf))
    (let ((info (gptel-fsm-info fsm)))
      (when (and (macher-agent-plist-p info)
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
        (macher-agent-set-fsm-info fsm info)))))

(defun macher-agent-wire--shadow-append-reasoning (wbuf fsm chunk)
  "Append reasoning CHUNK into shadow buffer WBUF for FSM."
  (when (and (bufferp wbuf) (buffer-live-p wbuf))
    (if (eq chunk t)
        (macher-agent-wire--shadow-close-reasoning-fence wbuf fsm)
      (when (and (stringp chunk) (not (string-empty-p chunk)))
        (let ((info (and fsm (gptel-fsm-info fsm))))
          (with-current-buffer wbuf
            (let ((inhibit-read-only t)
                  (inhibit-modification-hooks t)
                  (pt (point-max)))
              (save-excursion
                (goto-char pt)
                (unless (and (macher-agent-plist-p info)
                             (plist-get info :macher-agent-shadow-in-reasoning))
                  (unless (or (bobp) (eq (char-before) ?\n))
                    (insert "\n"))
                  (insert "``` reasoning\n")
                  (when (macher-agent-plist-p info)
                    (setq info (plist-put info :macher-agent-shadow-in-reasoning t))
                    (macher-agent-set-fsm-info fsm info)))
                (insert (substring-no-properties chunk))
                (put-text-property pt (point) 'gptel 'ignore)))))))))

(defun macher-agent-wire--shadow-append-tool-results (wbuf fsm tool-results info)
  "Append formatted TOOL-RESULTS into shadow buffer WBUF for FSM using INFO."
  (when (and (bufferp wbuf) (buffer-live-p wbuf) (listp tool-results) tool-results)
    (macher-agent-wire--shadow-close-reasoning-fence wbuf fsm)
    (let ((tool-use (and (macher-agent-plist-p info) (plist-get info :tool-use)))
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
            (when (and fsm (macher-agent-plist-p info))
              (let* ((cur-info (gptel-fsm-info fsm))
                     (cur-data (and (macher-agent-plist-p cur-info)
                                    (plist-get cur-info :data)))
                     (post-tret-len (length (macher-agent-existing-prompts-from-data cur-data))))
                (setq cur-info (plist-put cur-info :macher-agent-wire-shadow-tools-end (point)))
                (setq cur-info (plist-put cur-info :macher-agent-wire-shadow-final-written nil))
                (setq cur-info (plist-put cur-info :macher-agent-wire-wait-len post-tret-len))
                (macher-agent-set-fsm-info fsm cur-info)))))))))

(defun macher-agent-wire--shadow-append-response-chunk (wbuf fsm text)
  "Append assistant response TEXT into shadow buffer WBUF for FSM."
  (when (and (bufferp wbuf) (buffer-live-p wbuf)
             (stringp text) (not (string-empty-p text)))
    (macher-agent-wire--shadow-close-reasoning-fence wbuf fsm)
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
        (when (macher-agent-plist-p cur-info)
          (setq cur-info (plist-put cur-info :macher-agent-wire-shadow-final-written t))
          (macher-agent-set-fsm-info fsm cur-info))))))

(defun macher-agent-wire--make-wrapped-callback (actual-fsm target-buf inner-cb)
  "Build wire-recording FSM callback wrapping INNER-CB for ACTUAL-FSM.
Records reasoning, tool results, and response chunks into the shadow
wire buffer owned by TARGET-BUF."
  (lambda (&rest cb-args)
    (let* ((first-arg (car cb-args))
           (cb-info (cadr cb-args))
           (fsm-info (and actual-fsm (fboundp 'gptel-fsm-info)
                          (ignore-errors (gptel-fsm-info actual-fsm))))
           (eff-info (if (macher-agent-plist-p fsm-info) fsm-info cb-info))
           (wire-active (and target-buf
                             (macher-agent-wire--enabled-p target-buf eff-info)
                             (macher-agent-plist-p eff-info)
                             (plist-get eff-info :macher-agent-turn-started)
                             (not (plist-get eff-info :macher-agent-wire-turn-invalidated))))
           (wbuf (when wire-active
                   (macher-agent-wire-buffer-for target-buf t))))
      (cond
       ((and (consp first-arg) (eq (car first-arg) 'reasoning))
        (when wbuf
          (macher-agent-wire--shadow-append-reasoning wbuf actual-fsm (cdr first-arg)))
        (when (or (not wire-active)
                  (and (buffer-live-p target-buf)
                       (with-current-buffer target-buf
                         (bound-and-true-p gptel-include-reasoning))))
          (apply inner-cb cb-args)))
       ((and (consp first-arg) (eq (car first-arg) 'tool-result))
        (let ((raw-tr (cdr first-arg)))
          (when wbuf
            (macher-agent-wire--shadow-append-tool-results wbuf actual-fsm raw-tr cb-info))
          (apply inner-cb cb-args)
          (when (and wire-active actual-fsm (buffer-live-p target-buf))
            (let* ((cur-info (gptel-fsm-info actual-fsm))
                   (tm (and (macher-agent-plist-p cur-info)
                            (plist-get cur-info :tracking-marker)))
                   (pm (and (macher-agent-plist-p cur-info)
                            (plist-get cur-info :position)))
                   (orig-tend (cond
                               ((and (markerp tm) (eq (marker-buffer tm) target-buf))
                                (marker-position tm))
                               ((and (markerp pm) (eq (marker-buffer pm) target-buf))
                                (marker-position pm))
                               (t nil))))
              (when (and orig-tend (macher-agent-plist-p cur-info))
                (setq cur-info (plist-put cur-info :macher-agent-orig-tools-end orig-tend))
                (macher-agent-set-fsm-info actual-fsm cur-info))))))
       ((stringp first-arg)
        (when (and wbuf (null (nth 2 cb-args)))
          (macher-agent-wire--shadow-append-response-chunk wbuf actual-fsm first-arg))
        (apply inner-cb cb-args))
       (t
        (when wbuf
          (macher-agent-wire--shadow-close-reasoning-fence wbuf actual-fsm))
        (apply inner-cb cb-args))))))

;;; Shadow Span Parsing, Reconciliation, and Deduplication

(defun macher-agent-wire--parse-buffer-slice (buf beg end backend &optional strip-skills orig-ref-buf id-tables-cell)
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
                      (macher-agent-wire-strip-local-variables)
                      (when strip-skills
                        (macher-agent-extract-inline-skills
                         (point-min) (or orig-ref-buf buf)))
                      (let ((tables (macher-agent-assign-missing-buffer-tool-ids)))
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

(defun macher-agent-wire-collect-spans (wbuf)
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

(defun macher-agent-wire--find-shadow-span-bounds (wbuf span-id)
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

(defun macher-agent-wire--prune-shadow-spans (wbuf spans &optional orig-buf)
  "Delete SPANS from shadow wire buffer WBUF and detach unreferenced markers.
When ORIG-BUF (or `macher-agent-wire-origin-buffer') is live and any
span is removed, run `macher-agent-wire-invalidated-hook' with `pruned'."
  (when (and spans (bufferp wbuf) (buffer-live-p wbuf))
    (let ((target-orig (or orig-buf
                           (buffer-local-value 'macher-agent-wire-origin-buffer wbuf)))
          (removed nil))
      (with-current-buffer wbuf
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (candidate-markers nil)
              (still-used (make-hash-table :test 'eq))
              (ranges nil))
          (dolist (sp spans)
            (let* ((live (when-let* ((sid (plist-get sp :span-id)))
                           (macher-agent-wire--find-shadow-span-bounds wbuf sid)))
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
        (unless (macher-agent-wire-has-spans-p target-orig)
          (with-current-buffer target-orig
            (setq-local macher-agent-wire-horizon-turn-id nil)
            (setq-local macher-agent-wire-horizon-lines-deleted nil)
            (setq-local macher-agent-wire-dedup-turn-id nil)))
        (run-hook-with-args 'macher-agent-wire-invalidated-hook target-orig 'pruned)))))

(defun macher-agent-wire--insert-slice-with-shadow-props (wbuf orig-buf beg end kind)
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

(defun macher-agent-wire--update-shadow-span (wbuf span new-entries new-hash backend-type &optional orig-buf)
  "Update SPAN in WBUF with edited ORIG-BUF text and NEW-ENTRIES.
Stores NEW-HASH and BACKEND-TYPE on the updated span."
  (when (and (bufferp wbuf) (buffer-live-p wbuf))
    (let* ((target-orig (or orig-buf
                            (buffer-local-value 'macher-agent-wire-origin-buffer wbuf)))
           (live (when-let* ((sid (plist-get span :span-id)))
                   (macher-agent-wire--find-shadow-span-bounds wbuf sid)))
           (copied (macher-agent-wire-deep-copy-data new-entries))
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
                           'macher-agent-wire-chars (macher-agent-wire-estimate-chars copied)
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
                                 (macher-agent-wire--insert-slice-with-shadow-props
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

(defun macher-agent-wire--insert-shadow-gap-span (wbuf orig-buf beg end insert-pos entries turn-id &optional kind backend-type)
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
           (norm-h (macher-agent-wire-norm-hash orig-buf beg end))
           (nonempty (not (eql norm-h (sxhash-equal ""))))
           (copied (macher-agent-wire-deep-copy-data entries))
           (eff-kind (or kind 'gap))
           (chars (macher-agent-wire-estimate-chars copied)))
      (with-current-buffer wbuf
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (save-excursion
            (goto-char (max (point-min) (min (point-max) (or insert-pos (point-max)))))
            (unless (or (bobp) (eq (char-before) ?\n))
              (let ((nl-pt (point)))
                (insert "\n")
                (set-text-properties nl-pt (point) nil)))
            (setq-local macher-agent-wire--span-counter
                        (1+ macher-agent-wire--span-counter))
            (let* ((sid macher-agent-wire--span-counter)
                   (eff-tid (or turn-id (cons 'gap sid))))
              (pcase-let ((`(,nb . ,ne)
                           (macher-agent-wire--insert-slice-with-shadow-props
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

(defun macher-agent-wire--group-spans-by-turn (spans)
  "Group ordered SPANS list into chronological `(TURN-ID . SPAN-LIST)' cells."
  (let ((groups nil))
    (dolist (sp spans)
      (let* ((tid (plist-get sp :turn-id))
             (cell (assoc tid groups)))
        (if cell
            (setcdr cell (append (cdr cell) (list sp)))
          (push (cons tid (list sp)) groups))))
    (nreverse groups)))

(defun macher-agent-wire--span-live-in-buf-p (sp buf)
  "Return non-nil when SPAN SP has live markers attached to BUF."
  (let ((bm (plist-get sp :orig-beg))
        (em (plist-get sp :orig-end)))
    (and (markerp bm) (markerp em)
         (eq (marker-buffer bm) buf)
         (eq (marker-buffer em) buf))))

(defun macher-agent-wire--span-deleted-p (sp buf)
  "Return non-nil when SPAN SP was deleted or cleared to blank in BUF."
  (or (not (macher-agent-wire--span-live-in-buf-p sp buf))
      (and (plist-get sp :nonempty)
           (let ((b (marker-position (plist-get sp :orig-beg)))
                 (e (marker-position (plist-get sp :orig-end))))
             (or (>= b e)
                 (eql (macher-agent-wire-norm-hash buf b e)
                      (sxhash-equal "")))))))

(defun macher-agent-wire--format-truncation-alert-prompt (backend lines-deleted toolless)
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

(defun macher-agent-wire--restore-response-props (buf beg end)
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

(defun macher-agent-wire-sync-spans (orig-buf limit-pos effective-backend &optional id-tables-cell)
  "Synchronise shadow wire spans of ORIG-BUF up to LIMIT-POS.
Prunes deleted spans, updates edited spans in place, and materialises
manual gap insertions for EFFECTIVE-BACKEND into `wbuf'.  Optionally
records parsed ID tables in ID-TABLES-CELL."
  (when (and (bufferp orig-buf) (buffer-live-p orig-buf))
    (let* ((wbuf (macher-agent-wire-buffer-for orig-buf nil))
           (target-btype (and effective-backend (type-of effective-backend)))
           (spans (and wbuf (macher-agent-wire-collect-spans wbuf)))
           (groups (macher-agent-wire--group-spans-by-turn spans))
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
           ((not (and anchor-sp (macher-agent-wire--span-live-in-buf-p anchor-sp orig-buf)))
            (setq to-prune (append to-prune gspans)))
           ((and u-sp (macher-agent-wire--span-deleted-p u-sp orig-buf))
            (setq to-prune (append to-prune gspans)))
           ((and standalone-gap-p (macher-agent-wire--span-deleted-p g-sp orig-buf))
            (setq to-prune (append to-prune gspans)))
           ((>= (marker-position (plist-get anchor-sp :orig-beg)) limit-pos)
            (dolist (sp gspans)
              (when (macher-agent-wire--span-deleted-p sp orig-buf)
                (push sp to-prune)))
            (setq stop-after-limit t))
           ((not stop-after-limit)
            (let ((f-deleted (and f-sp (macher-agent-wire--span-deleted-p f-sp orig-buf))))
              (dolist (sp gspans)
                (let* ((sp-kind (plist-get sp :kind))
                       (sp-live (macher-agent-wire--span-live-in-buf-p sp orig-buf))
                       (sp-deleted
                        (or (not sp-live)
                            (macher-agent-wire--span-deleted-p sp orig-buf)
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
                                     (macher-agent-wire--parse-buffer-slice
                                      orig-buf prev-orig-pos sp-beg effective-backend t orig-buf id-tables-cell))
                                    (ins-pos (or (car-safe (macher-agent-wire--find-shadow-span-bounds
                                                            wbuf (plist-get sp :span-id)))
                                                 (plist-get sp :sbeg))))
                          (macher-agent-wire--insert-shadow-gap-span
                           wbuf orig-buf prev-orig-pos sp-beg ins-pos gap-prompts gap-tid 'gap target-btype))
                        (setq prev-orig-pos sp-beg))
                      (when (and (= sp-end-c sp-end)
                                 (eq (plist-get sp :backend-type) target-btype))
                        (let ((cur-hash (macher-agent-wire-norm-hash orig-buf sp-beg sp-end-c)))
                          (unless (eql cur-hash (plist-get sp :norm-hash))
                            (unless is-prompt-like
                              (macher-agent-wire--restore-response-props orig-buf sp-beg sp-end-c))
                            (when-let* ((reparsed (macher-agent-wire--parse-buffer-slice
                                                   orig-buf sp-beg sp-end-c
                                                   effective-backend is-prompt-like orig-buf id-tables-cell)))
                              (macher-agent-wire--update-shadow-span
                               wbuf sp reparsed cur-hash target-btype orig-buf)))))
                      (setq prev-orig-pos (max prev-orig-pos sp-end-c))))))))))))
      (when (< prev-orig-pos limit-pos)
        (when-let* ((tail-gap (macher-agent-wire--parse-buffer-slice
                               orig-buf prev-orig-pos limit-pos effective-backend t orig-buf id-tables-cell)))
          (unless wbuf
            (setq wbuf (macher-agent-wire-buffer-for orig-buf t)))
          (macher-agent-wire--insert-shadow-gap-span
           wbuf orig-buf prev-orig-pos limit-pos (with-current-buffer wbuf (point-max))
           tail-gap nil 'gap target-btype)))
      (when (and wbuf to-prune)
        (macher-agent-wire--prune-shadow-spans wbuf (delete-dups to-prune) orig-buf)))))

(defun macher-agent-wire--canonical-tool-args (args)
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

(defun macher-agent-wire--collect-dedup-sites (entries)
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
                              (let ((sig (cons fname (macher-agent-wire--canonical-tool-args
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
                          (puthash id (cons name (macher-agent-wire--canonical-tool-args
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
                            (puthash id (cons name (macher-agent-wire--canonical-tool-args args))
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
              (puthash cid (cons name (macher-agent-wire--canonical-tool-args
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

(defun macher-agent-wire-deduplicate-bundles (bundles up-to-turn-id)
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
                                        (macher-agent-wire-deep-copy-data
                                         (plist-get b :entries))))
                         b)))
                   bundles)))
      (dolist (b (reverse copied-bundles))
        (let ((tid (plist-get b :turn-id)))
          (when (and (integerp tid) (<= tid up-to-turn-id))
            (let ((sites (macher-agent-wire--collect-dedup-sites (plist-get b :entries)))
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
                           (macher-agent-wire-estimate-chars
                            (plist-get b :entries))))))))
      copied-bundles)))

(defun macher-agent-wire-build-turn-bundles (wbuf &optional orig-buf limit-pos effective-backend id-tables-cell)
  "Build chronological turn bundles from WBUF up to LIMIT-POS in ORIG-BUF.
Each bundle is a plist `(:turn-id :entries :chars :sbeg :send)'.  When
EFFECTIVE-BACKEND is nil, reuses stored span entries directly and passes
optional ID-TABLES-CELL when re-parsing slices."
  (when (and (bufferp wbuf) (buffer-live-p wbuf))
    (let* ((spans (macher-agent-wire-collect-spans wbuf))
           (groups (macher-agent-wire--group-spans-by-turn spans))
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
                          (macher-agent-wire--span-live-in-buf-p anchor-sp orig-buf))))
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
                       (deleted (and orig-buf (macher-agent-wire--span-deleted-p sp orig-buf))))
                  (when (and (not deleted)
                             (or (null limit-pos) (null ob-pos) (<= ob-pos limit-pos)))
                    (let* ((same-b (or (null target-btype)
                                       (eq (plist-get sp :backend-type) target-btype)))
                           (full-slice (or (null limit-pos) (null oe-pos) (= oe-clamped oe-pos)))
                           (hash-ok (or (null orig-buf)
                                        (null ob-pos)
                                        (null oe-clamped)
                                        (eql (macher-agent-wire-norm-hash orig-buf ob-pos oe-clamped)
                                             (plist-get sp :norm-hash)))))
                      (if (and same-b full-slice hash-ok)
                          (progn
                            (setq turn-entries
                                  (append turn-entries
                                          (macher-agent-wire-deep-copy-data
                                           (plist-get sp :entries))))
                            (setq turn-chars (+ turn-chars (or (plist-get sp :chars) 0))))
                        (when (and orig-buf effective-backend ob-pos oe-clamped)
                          (when (memq sp-kind '(tool-step model-final))
                            (macher-agent-wire--restore-response-props orig-buf ob-pos oe-clamped))
                          (let ((reparsed
                                 (macher-agent-wire--parse-buffer-slice
                                  orig-buf ob-pos oe-clamped effective-backend
                                  (not (memq sp-kind '(tool-step model-final)))
                                  orig-buf id-tables-cell)))
                            (setq turn-entries (append turn-entries reparsed))
                            (setq turn-chars
                                  (+ turn-chars
                                     (macher-agent-wire-estimate-chars reparsed))))))))))
              (when turn-entries
                (push (list :turn-id tid
                            :entries turn-entries
                            :chars turn-chars
                            :sbeg turn-sbeg
                            :send turn-send)
                      bundles)))))))
      (nreverse bundles))))

(defun macher-agent-wire-resolve-horizon (orig-buf bundles max-chars)
  "Resolve quantised truncation horizon and dedup epoch for ORIG-BUF.
Inspects chronological BUNDLES against MAX-CHARS and returns
`(:retained :horizon-send :lines-deleted)'."
  (if (or (null bundles) (null max-chars) (<= max-chars 0))
      (list :retained bundles :horizon-send nil :lines-deleted nil)
    (let* ((wbuf (macher-agent-wire-buffer-for orig-buf nil))
           (total-all (cl-loop for b in bundles sum (or (plist-get b :chars) 0)))
           (horizon (buffer-local-value 'macher-agent-wire-horizon-turn-id orig-buf))
           (pinned-lines (buffer-local-value 'macher-agent-wire-horizon-lines-deleted orig-buf))
           (dedup-tid (buffer-local-value 'macher-agent-wire-dedup-turn-id orig-buf))
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
          (setq-local macher-agent-wire-horizon-turn-id nil)
          (setq-local macher-agent-wire-horizon-lines-deleted nil)
          (setq-local macher-agent-wire-dedup-turn-id nil)))
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
        (when (and dedup-tid
                   macher-agent-max-duplicate-tools
                   (> macher-agent-max-duplicate-tools 0))
          (setq retained (macher-agent-wire-deduplicate-bundles retained dedup-tid)))
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
                  (setq-local macher-agent-wire-dedup-turn-id dedup-tid))
                (setq retained (macher-agent-wire-deduplicate-bundles retained dedup-tid))
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
                         (clamped-send (and wbuf (buffer-live-p wbuf) (integerp dropped-send)
                                            (with-current-buffer wbuf
                                              (max (point-min) (min (point-max) dropped-send)))))
                         (new-lines
                          (if clamped-send
                              (with-current-buffer wbuf
                                (max 1 (count-lines (point-min) clamped-send)))
                            1))
                         (shadow-line
                          (if clamped-send
                              (with-current-buffer wbuf
                                (line-number-at-pos clamped-send))
                            new-lines)))
                    (setq horizon new-horizon
                          pinned-lines new-lines)
                    (with-current-buffer orig-buf
                      (setq-local macher-agent-wire-horizon-turn-id new-horizon)
                      (setq-local macher-agent-wire-horizon-lines-deleted new-lines)
                      (setq-local macher-agent-wire-dedup-turn-id dedup-tid))
                    (run-hook-with-args 'macher-agent-wire-pre-truncate-hook
                                        orig-buf dropped-send shadow-line)))))))
        (list :retained retained
              :horizon-send dropped-send
              :lines-deleted (or pinned-lines
                                 (buffer-local-value 'macher-agent-wire-horizon-lines-deleted orig-buf)))))))

(defun macher-agent-wire-horizon-bounds (orig-buf &optional max-chars)
  "Return wire-managed event horizon plist for ORIG-BUF within MAX-CHARS.
Returns `(:line L :offset O :dedup-line DL :dedup-offset DO :tick T
:wire-managed t)' when ORIG-BUF owns committed shadow spans, or nil.
When MAX-CHARS is nil and a horizon or dedup epoch is already pinned on
ORIG-BUF, return the pinned bounds without clearing or mutating them."
  (when (and (bufferp orig-buf) (buffer-live-p orig-buf)
             (macher-agent-wire-has-spans-p orig-buf))
    (when-let* ((wbuf (macher-agent-wire-buffer-for orig-buf nil)))
      (let* ((pinned-horizon (buffer-local-value 'macher-agent-wire-horizon-turn-id orig-buf))
             (pinned-dedup (buffer-local-value 'macher-agent-wire-dedup-turn-id orig-buf))
             (read-only-pinned (and (null max-chars)
                                    (or pinned-horizon pinned-dedup)))
             (horizon-send
              (if read-only-pinned
                  (when pinned-horizon
                    (with-current-buffer wbuf
                      (let ((pos (point-min))
                            (max-end nil))
                        (while (< pos (point-max))
                          (let ((tid (get-text-property pos 'macher-agent-turn-id))
                                (next (next-single-property-change
                                       pos 'macher-agent-span-id wbuf (point-max))))
                            (when (cond
                                   ((integerp pinned-horizon)
                                    (and (integerp tid) (<= tid pinned-horizon)))
                                   ((consp pinned-horizon)
                                    (equal tid pinned-horizon))
                                   (t nil))
                              (setq max-end next))
                            (setq pos next)))
                        max-end)))
                (let* ((active-model
                        (when (null max-chars)
                          (let ((fsm (macher-agent-get-active-fsm)))
                            (when fsm
                              (ignore-errors
                                (plist-get (macher-agent-extract-fsm-info fsm) :model))))))
                       (effective-max (or max-chars
                                          (macher-agent-get-max-context-chars orig-buf active-model)))
                       (wire-backend (buffer-local-value 'macher-agent-wire-backend orig-buf))
                       (bundles (macher-agent-wire-build-turn-bundles
                                 wbuf orig-buf nil wire-backend))
                       (horizon-res (macher-agent-wire-resolve-horizon orig-buf bundles effective-max)))
                  (plist-get horizon-res :horizon-send))))
             (tick (+ (buffer-chars-modified-tick orig-buf)
                      (buffer-chars-modified-tick wbuf)))
             (h-bounds
              (if (integerp horizon-send)
                  (with-current-buffer wbuf
                    (let ((clamped (max (point-min) (min (point-max) horizon-send))))
                      (cons (line-number-at-pos clamped) clamped)))
                (cons 1 1)))
             (dedup-tid (buffer-local-value 'macher-agent-wire-dedup-turn-id orig-buf))
             (dedup-bounds
              (when (integerp dedup-tid)
                (with-current-buffer wbuf
                  (let ((pos (point-min))
                        (max-end nil))
                    (while (< pos (point-max))
                      (let ((tid (get-text-property pos 'macher-agent-turn-id))
                            (next (next-single-property-change
                                   pos 'macher-agent-span-id wbuf (point-max))))
                        (when (and (integerp tid) (<= tid dedup-tid))
                          (setq max-end next))
                        (setq pos next)))
                    (when (integerp max-end)
                      (let ((clamped (max (point-min) (min (point-max) max-end))))
                        (cons (line-number-at-pos clamped) clamped))))))))
        (list :line (car h-bounds)
              :offset (cdr h-bounds)
              :dedup-line (car-safe dedup-bounds)
              :dedup-offset (cdr-safe dedup-bounds)
              :tick tick
              :wire-managed t)))))

(defun macher-agent-wire--reconcile-history (orig-buf limit-pos effective-backend disable-local-tools &optional max-turns max-chars id-tables-cell)
  "Reconcile shadow wire history of ORIG-BUF up to LIMIT-POS.
Formats prompts for EFFECTIVE-BACKEND, flattening tools when
DISABLE-LOCAL-TOOLS is non-nil, and bounding turns by MAX-TURNS and
MAX-CHARS.  Stores parsed ID tables in ID-TABLES-CELL when provided."
  (macher-agent-wire-sync-spans orig-buf limit-pos effective-backend id-tables-cell)
  (let* ((wbuf (macher-agent-wire-buffer-for orig-buf nil))
         (target-btype (and effective-backend (type-of effective-backend)))
         (turn-bundles (and wbuf
                            (macher-agent-wire-build-turn-bundles
                             wbuf orig-buf limit-pos effective-backend id-tables-cell)))
         (horizon-res (macher-agent-wire-resolve-horizon orig-buf turn-bundles max-chars))
         (retained (plist-get horizon-res :retained))
         (horizon-send (plist-get horizon-res :horizon-send))
         (lines-del (plist-get horizon-res :lines-deleted)))
    (when (and (integerp max-turns) (> max-turns 0))
      (let ((keep (max 0 (1- max-turns))))
        (when (> (length retained) keep)
          (setq retained (last retained keep)))))
    (let* ((truncated-alert
            (when (and horizon-send lines-del)
              (macher-agent-wire--format-truncation-alert-prompt
               effective-backend (max 1 lines-del) disable-local-tools)))
           (flat (cl-loop for b in retained append (plist-get b :entries))))
      (when truncated-alert
        (setq flat (cons truncated-alert flat)))
      (if (and disable-local-tools
               (eq target-btype 'gptel-gemini))
          (macher-agent-sanitize-prompts-for-toolless flat)
        flat))))

(defun macher-agent-wire-rebuild-prompts (buf backend tool-use &optional info)
  "Rebuild BACKEND prompt list from shadow spans of BUF and open TOOL-USE.
Uses optional FSM INFO and falls back to
`macher-agent-build-prompts-from-buffer' when BUF has no matching shadow
wire spans."
  (if (and (bufferp buf) (buffer-live-p buf)
           (macher-agent-wire-has-spans-p buf)
           (eq backend (buffer-local-value 'macher-agent-wire-backend buf)))
      (let* ((tracking (and (macher-agent-plist-p info) (plist-get info :tracking-marker)))
             (pos (and (macher-agent-plist-p info) (plist-get info :position)))
             (end-pos (or (and (markerp tracking) (eq (marker-buffer tracking) buf)
                               (marker-position tracking))
                          (and (markerp pos) (eq (marker-buffer pos) buf)
                               (marker-position pos))))
             (id-tables-cell (list nil))
             (pbounds (and (macher-agent-plist-p info)
                           (plist-get info :macher-agent-prompt-bounds)))
             (limit (or (car-safe pbounds)
                        (with-current-buffer buf
                          (macher-agent-prompt-start-pos (or end-pos (point-max))))))
             (disable-local-tools (and (macher-agent-plist-p info)
                                       (plist-get info :disable-local-tools)))
             (max-chars (macher-agent-get-max-context-chars
                         buf (and (macher-agent-plist-p info)
                                  (plist-get info :model))))
             (hist (macher-agent-wire--reconcile-history
                    buf limit backend disable-local-tools
                    (with-current-buffer buf (bound-and-true-p gptel--num-messages-to-send))
                    max-chars id-tables-cell))
             (active-tail
              (macher-agent-wire--parse-buffer-slice
               buf limit (or end-pos (with-current-buffer buf (point-max)))
               backend t buf id-tables-cell))
             (prompts (append hist active-tail))
             (id-tables (car id-tables-cell))
             (_ (macher-agent-ensure-tool-use-ids
                 tool-use (car id-tables) (cdr id-tables)))
             (open-prompts
              (when (and tool-use (fboundp 'gptel--parse-list-and-insert))
                (macher-agent-merge-parallel-open-prompts
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
        (append prompts open-prompts))
    (macher-agent-build-prompts-from-buffer buf backend tool-use info)))

;;; FSM Lifecycle Handlers

(defun macher-agent-wire--apply-prefix-to-fsm (fsm)
  "Splice pending `:macher-agent-wire-prefix' into FSM `:data' idempotently."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let ((info (gptel-fsm-info fsm)))
      (when (and (macher-agent-plist-p info)
                 (plist-member info :macher-agent-wire-prefix)
                 (listp (plist-get info :data))
                 (keywordp (car (plist-get info :data))))
        (let* ((prefix (plist-get info :macher-agent-wire-prefix))
               (backend (plist-get info :backend))
               (data (plist-get info :data))
               (new-info (copy-sequence info))
               (user-len (length (macher-agent-existing-prompts-from-data data))))
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
              (gptel--inject-prompt backend data (macher-agent-wire-deep-copy-data prefix) inject-pos)))
          (setq new-info (plist-put new-info :macher-agent-wire-prefix-len (length prefix)))
          (setq new-info (plist-put new-info :macher-agent-wire-user-len user-len))
          (macher-agent-set-fsm-info fsm new-info)
          (setf (gptel-fsm-info fsm) new-info)))))
  fsm)

(defun macher-agent-wire-finalize-transform (async-fn fsm)
  "Invoke ASYNC-FN and splice pending wire prefix onto FSM `:data'."
  (when (functionp async-fn)
    (funcall async-fn))
  (when fsm
    (macher-agent-wire--apply-prefix-to-fsm fsm)))

(defun macher-agent-wire--handle-wait (fsm)
  "Prepare shadow wire state and callback wrapping on FSM entry to `WAIT'."
  (macher-agent-wire--apply-prefix-to-fsm fsm)
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (target-buf (and (macher-agent-plist-p info) (plist-get info :buffer))))
      (when (and (macher-agent-plist-p info)
                 (not (plist-get info :macher-agent-wire-turn-invalidated))
                 (macher-agent-wire--enabled-p target-buf info))
        (if (plist-get info :macher-agent-turn-started)
            (let* ((new-info (copy-sequence info))
                   (data (plist-get new-info :data))
                   (wait-len (length (macher-agent-existing-prompts-from-data data))))
              (dolist (k '(:reasoning-block :reasoning-marker :partial_reasoning
                                            :signature))
                (when (plist-member new-info k)
                  (setq new-info (plist-put new-info k nil))))
              (setq new-info (plist-put new-info :macher-agent-wire-wait-len wait-len))
              (macher-agent-set-fsm-info fsm new-info)
              (setf (gptel-fsm-info fsm) new-info))
          (let* ((wbuf (macher-agent-wire-buffer-for target-buf t))
                 (data (plist-get info :data))
                 (prefix-len (or (plist-get info :macher-agent-wire-prefix-len) 0))
                 (total-now (length (macher-agent-existing-prompts-from-data data)))
                 (user-len (or (plist-get info :macher-agent-wire-user-len)
                               (max 0 (- total-now prefix-len))))
                 (pbounds (plist-get info :macher-agent-prompt-bounds))
                 (pos-m (plist-get info :position))
                 (pbeg (or (car-safe pbounds)
                           (with-current-buffer target-buf
                             (macher-agent-prompt-start-pos
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
            (unless (plist-get new-info :macher-agent-wire-cb-wrapped)
              (let* ((base-cb (or (plist-get new-info :callback)
                                  (if (and (plist-get new-info :stream)
                                           (bound-and-true-p gptel-use-curl))
                                      #'gptel-curl--stream-insert-response
                                    #'gptel--insert-response)))
                     (core-cb (if (plist-get new-info :macher-agent-cb-wrapped)
                                  base-cb
                                (macher-agent-wrap-fsm-callback fsm target-buf base-cb)))
                     (wrapped (macher-agent-wire--make-wrapped-callback fsm target-buf core-cb)))
                (setq new-info (plist-put new-info :callback wrapped))
                (setq new-info (plist-put new-info :macher-agent-cb-wrapped t))
                (setq new-info (plist-put new-info :macher-agent-wire-cb-wrapped t))))
            (macher-agent-set-fsm-info fsm new-info)
            (setf (gptel-fsm-info fsm) new-info)))))))

(defun macher-agent-wire--ensure-final-entry (fsm final-text)
  "Ensure FSM `:data' contains the final assistant message for FINAL-TEXT."
  (let* ((info (gptel-fsm-info fsm))
         (backend (and (macher-agent-plist-p info) (plist-get info :backend)))
         (data (and (macher-agent-plist-p info) (plist-get info :data)))
         (wait-len (or (plist-get info :macher-agent-wire-wait-len) 0))
         (cur-len (length (macher-agent-existing-prompts-from-data data))))
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

(defun macher-agent-wire--extract-shadow-final-text (wbuf from-pos)
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

(defun macher-agent-wire-annotate-shadow-span (wbuf sbeg send props)
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
        (setq-local macher-agent-wire--span-counter (1+ macher-agent-wire--span-counter))
        (add-text-properties
         b e
         (append (list 'macher-agent-span-id macher-agent-wire--span-counter)
                 props)))
      e)))

(defun macher-agent-wire--capture-done (fsm)
  "Commit lossless wire entries and markers for FSM into shadow wire buffer."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (target-buf (and (macher-agent-plist-p info) (plist-get info :buffer))))
      (when (and (macher-agent-plist-p info)
                 (plist-get info :macher-agent-turn-started)
                 (not (plist-get info :macher-agent-wire-turn-invalidated))
                 (not (plist-get info :macher-agent-wire-done-captured))
                 (macher-agent-wire--enabled-p target-buf info))
        (let* ((wbuf (macher-agent-wire-buffer-for target-buf t))
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
                                 (macher-agent-extract-turn-response-text b e)
                               ""))))
               (raw-md-text (when (plist-get info :macher-agent-wire-shadow-final-written)
                              (macher-agent-wire--extract-shadow-final-text wbuf s-tend)))
               (wire-final-text (if (and (stringp raw-md-text)
                                         (not (string-empty-p raw-md-text)))
                                    raw-md-text
                                  vis-text)))
          (macher-agent-wire--shadow-close-reasoning-fence wbuf fsm)
          (macher-agent-wire--ensure-final-entry fsm wire-final-text)
          (let* ((data (plist-get (gptel-fsm-info fsm) :data))
                 (all-prompts (macher-agent-existing-prompts-from-data data))
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
                 (user-entries (macher-agent-wire-deep-copy-data
                                (seq-subseq all-prompts prefix-len tools-start)))
                 (tool-entries (macher-agent-wire-deep-copy-data
                                (seq-subseq all-prompts tools-start wait-len)))
                 (final-entries (macher-agent-wire-deep-copy-data
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
                              (macher-agent-wire-norm-hash target-buf pbeg-m pend-m)))
                 (u-nonempty (and u-hash (not (eql u-hash (sxhash-equal "")))))
                 (t-hash (macher-agent-wire-norm-hash target-buf rbeg-m orig-tend-m))
                 (t-nonempty (not (eql t-hash (sxhash-equal ""))))
                 (f-hash (macher-agent-wire-norm-hash target-buf orig-tend-m rend-m))
                 (f-nonempty (not (eql f-hash (sxhash-equal ""))))
                 (r-nonempty (or t-nonempty f-nonempty))
                 (turn-id (with-current-buffer target-buf
                            (setq-local macher-agent-wire-backend backend)
                            (setq-local macher-agent-wire-turn-counter
                                        (1+ macher-agent-wire-turn-counter)))))
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
                  (setq-local macher-agent-wire--last-media-hash (sxhash-equal mlist)))))
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
                     (macher-agent-wire-annotate-shadow-span
                      wbuf s-start s-uend
                      (list 'macher-agent-turn-id turn-id
                            'macher-agent-span-kind 'user-prompt
                            'macher-agent-wire-entries user-entries
                            'macher-agent-wire-backend-type btype
                            'macher-agent-wire-chars (macher-agent-wire-estimate-chars user-entries)
                            'macher-agent-orig-beg pbeg-m
                            'macher-agent-orig-end pend-m
                            'macher-agent-orig-nonempty u-nonempty
                            'macher-agent-norm-hash u-hash))))
                (when (= s-tend s-uend)
                  (setq s-tend new-uend))
                (setq s-uend new-uend)))
            (when (and tool-entries (integerp s-uend) (integerp s-tend))
              (setq s-tend
                    (macher-agent-wire-annotate-shadow-span
                     wbuf s-uend s-tend
                     (list 'macher-agent-turn-id turn-id
                           'macher-agent-span-kind 'tool-step
                           'macher-agent-wire-entries tool-entries
                           'macher-agent-wire-backend-type btype
                           'macher-agent-wire-chars (macher-agent-wire-estimate-chars tool-entries)
                           'macher-agent-orig-beg rbeg-m
                           'macher-agent-orig-end orig-tend-m
                           'macher-agent-prompt-beg pbeg-m
                           'macher-agent-prompt-end pend-m
                           'macher-agent-orig-nonempty t-nonempty
                           'macher-agent-norm-hash t-hash))))
            (when (and final-entries (integerp s-tend))
              (macher-agent-wire-annotate-shadow-span
               wbuf s-tend (with-current-buffer wbuf (point-max))
               (list 'macher-agent-turn-id turn-id
                     'macher-agent-span-kind 'model-final
                     'macher-agent-wire-entries final-entries
                     'macher-agent-wire-backend-type btype
                     'macher-agent-wire-chars (macher-agent-wire-estimate-chars final-entries)
                     'macher-agent-orig-beg orig-tend-m
                     'macher-agent-orig-end rend-m
                     'macher-agent-prompt-beg pbeg-m
                     'macher-agent-prompt-end rend-m
                     'macher-agent-orig-nonempty f-nonempty
                     'macher-agent-norm-hash f-hash)))
            (let ((done-info (copy-sequence (gptel-fsm-info fsm))))
              (setq done-info (plist-put done-info :macher-agent-wire-done-captured t))
              (when r-nonempty
                (setq done-info (plist-put done-info :macher-agent-response-end-marker rend-m)))
              (macher-agent-set-fsm-info fsm done-info)
              (setf (gptel-fsm-info fsm) done-info))))))))

(defun macher-agent-wire--finalize-done (fsm)
  "Lock response end-marker insertion type on FSM after `DONE' separators."
  (when (and fsm (fboundp 'gptel-fsm-state) (eq (gptel-fsm-state fsm) 'DONE))
    (when-let* ((info (ignore-errors (gptel-fsm-info fsm)))
                ((macher-agent-plist-p info))
                (target-buf (plist-get info :buffer))
                ((buffer-live-p target-buf))
                (rend-m (plist-get info :macher-agent-response-end-marker)))
      (with-current-buffer target-buf
        (when (and (markerp rend-m)
                   (eq (marker-buffer rend-m) target-buf)
                   (< (marker-position rend-m) (point-max)))
          (set-marker-insertion-type rend-m t))))))

(defun macher-agent-wire--rollback-turn (fsm)
  "Roll back uncommitted shadow wire turn output on FSM `ERRS' or `ABRT'."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (target-buf (and (macher-agent-plist-p info) (plist-get info :buffer))))
      (when (and (macher-agent-plist-p info)
                 (plist-get info :macher-agent-turn-started)
                 (not (plist-get info :macher-agent-wire-done-captured)))
        (when-let* ((pmarkers (plist-get info :macher-agent-prompt-markers)))
          (when (markerp (car-safe pmarkers)) (set-marker (car pmarkers) nil))
          (when (markerp (cdr-safe pmarkers)) (set-marker (cdr pmarkers) nil)))
        (when-let* ((wbuf (and target-buf (macher-agent-wire-buffer-for target-buf nil)))
                    (s-start (plist-get info :macher-agent-wire-shadow-turn-start)))
          (with-current-buffer wbuf
            (let ((inhibit-read-only t)
                  (inhibit-modification-hooks t)
                  (b (max (point-min) (min (point-max) s-start))))
              (when (< b (point-max))
                (macher-agent-wire--clear-buffer-markers wbuf b (point-max))
                (delete-region b (point-max))))))
        (let ((clean-info (plist-put (copy-sequence info)
                                     :macher-agent-turn-started nil)))
          (macher-agent-set-fsm-info fsm clean-info)
          (setf (gptel-fsm-info fsm) clean-info))))))

;;; Branch Cloning, Clearing, and Interactive Inspection

(defun macher-agent-wire-clone-buffer (parent-buf child-buf)
  "Clone shadow wire buffer from PARENT-BUF into CHILD-BUF with fresh markers."
  (when (and (bufferp parent-buf) (buffer-live-p parent-buf)
             (bufferp child-buf) (buffer-live-p child-buf)
             (macher-agent-wire-has-spans-p parent-buf))
    (when-let* ((parent-wbuf (macher-agent-wire-buffer-for parent-buf nil))
                (child-wbuf (macher-agent-wire-buffer-for child-buf t)))
      (let ((raw-copy (with-current-buffer parent-wbuf (buffer-string)))
            (turn-cnt (buffer-local-value 'macher-agent-wire-turn-counter parent-buf))
            (horizon-tid (buffer-local-value 'macher-agent-wire-horizon-turn-id parent-buf))
            (horizon-lines (buffer-local-value 'macher-agent-wire-horizon-lines-deleted parent-buf))
            (dedup-tid (buffer-local-value 'macher-agent-wire-dedup-turn-id parent-buf))
            (span-cnt (buffer-local-value 'macher-agent-wire--span-counter parent-wbuf))
            (media-h (buffer-local-value 'macher-agent-wire--last-media-hash parent-buf))
            (wire-b (buffer-local-value 'macher-agent-wire-backend parent-buf))
            (marker-map (make-hash-table :test 'eq)))
        (with-current-buffer child-buf
          (setq-local macher-agent-wire-turn-counter turn-cnt)
          (setq-local macher-agent-wire-horizon-turn-id horizon-tid)
          (setq-local macher-agent-wire-horizon-lines-deleted horizon-lines)
          (setq-local macher-agent-wire-dedup-turn-id dedup-tid)
          (setq-local macher-agent-wire--last-media-hash media-h)
          (setq-local macher-agent-wire-backend wire-b))
        (with-current-buffer child-wbuf
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (erase-buffer)
            (insert raw-copy)
            (setq-local macher-agent-wire--span-counter span-cnt)
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
                       (macher-agent-wire-deep-copy-data ents))))
                  (setq pos next))))))))))

(defun macher-agent-wire-clear-buffer (&optional buffer)
  "Clear committed spans and markers in shadow wire buffer for BUFFER."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local macher-agent-wire--last-media-hash nil)
        (setq-local macher-agent-wire-backend nil)
        (setq-local macher-agent-wire-turn-counter 0)
        (setq-local macher-agent-wire-horizon-turn-id nil)
        (setq-local macher-agent-wire-horizon-lines-deleted nil)
        (setq-local macher-agent-wire-dedup-turn-id nil)
        (when-let* ((fsm (bound-and-true-p macher-agent-active-fsm))
                    ((fboundp 'gptel-fsm-info))
                    (info (ignore-errors (gptel-fsm-info fsm)))
                    ((macher-agent-plist-p info)))
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
            (macher-agent-set-fsm-info fsm info)
            (setf (gptel-fsm-info fsm) info))))
      (when-let* ((wbuf (macher-agent-wire-buffer-for buf nil)))
        (macher-agent-wire--clear-buffer-markers wbuf)
        (with-current-buffer wbuf
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (erase-buffer)
            (setq-local macher-agent-wire--span-counter 0))))
      (run-hook-with-args 'macher-agent-wire-invalidated-hook buf 'cleared))))

(defun macher-agent-wire-inspect-buffer (&optional buffer)
  "Display the hidden shadow wire buffer associated with BUFFER."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (wbuf (macher-agent-wire-buffer-for buf nil)))
    (if (not wbuf)
        (user-error "No shadow wire buffer recorded for %s" (buffer-name buf))
      (pop-to-buffer wbuf))))

;;; Core Hook Adapters

(defun macher-agent-wire--insert-before-gptel-wait (handler funcs)
  "Insert HANDLER into WAIT FUNCS right before `gptel--handle-wait'."
  (if (memq handler funcs)
      funcs
    (let ((acc nil)
          (inserted nil))
      (dolist (fn funcs)
        (when (and (not inserted) (eq fn #'gptel--handle-wait))
          (push handler acc)
          (setq inserted t))
        (push fn acc))
      (unless inserted
        (push handler acc))
      (nreverse acc))))

(defun macher-agent-wire--on-fsm-hijack (fsm target-buf)
  "Install wire callback wrapper and state handlers on FSM for TARGET-BUF."
  (when fsm
    (let* ((inf (gptel-fsm-info fsm))
           (orig-cb (and (macher-agent-plist-p inf) (plist-get inf :callback))))
      (when (and orig-cb (not (plist-get inf :macher-agent-wire-cb-wrapped)))
        (let ((wrapped (macher-agent-wire--make-wrapped-callback fsm target-buf orig-cb)))
          (setq inf (plist-put (copy-sequence inf) :callback wrapped))
          (setq inf (plist-put inf :macher-agent-wire-cb-wrapped t))
          (macher-agent-set-fsm-info fsm inf)
          (setf (gptel-fsm-info fsm) inf))))
    (let* ((handlers (gptel-fsm-handlers fsm))
           (all-states (delete-dups (append (mapcar #'car handlers) '(INIT WAIT DONE ERRS ABRT))))
           (augmented
            (cl-loop for state in all-states
                     for funcs = (alist-get state handlers)
                     collect (cons state
                                   (cond
                                    ((eq state 'INIT)
                                     (if (memq #'macher-agent-wire--apply-prefix-to-fsm funcs)
                                         funcs
                                       (append funcs (list #'macher-agent-wire--apply-prefix-to-fsm))))
                                    ((eq state 'WAIT)
                                     (let ((f (macher-agent-wire--insert-before-gptel-wait
                                               #'macher-agent-wire--handle-wait funcs)))
                                       (unless (memq #'macher-agent-wire--apply-prefix-to-fsm f)
                                         (setq f (cons #'macher-agent-wire--apply-prefix-to-fsm f)))
                                       f))
                                    ((eq state 'DONE)
                                     (let ((f funcs))
                                       (unless (memq #'macher-agent-wire--capture-done f)
                                         (setq f (cons #'macher-agent-wire--capture-done f)))
                                       (unless (memq #'macher-agent-wire--finalize-done f)
                                         (setq f (append f (list #'macher-agent-wire--finalize-done))))
                                       f))
                                    ((memq state '(ERRS ABRT))
                                     (if (memq #'macher-agent-wire--rollback-turn funcs)
                                         funcs
                                       (cons #'macher-agent-wire--rollback-turn funcs)))
                                    (t funcs))))))
      (macher-agent-set-fsm-handlers fsm augmented)
      (setf (gptel-fsm-handlers fsm) augmented))))

(defun macher-agent-wire--on-pre-prompt-compile (active-fsm temp-buf orig-buf info)
  "Synchronise shadow spans on ORIG-BUF prior to prompt compilation.
Runs for ACTIVE-FSM, TEMP-BUF, ORIG-BUF, and FSM INFO."
  (when (and active-fsm
             (not (eq temp-buf orig-buf))
             (buffer-live-p orig-buf)
             (macher-agent-wire--enabled-p orig-buf info)
             (eq (buffer-local-value 'major-mode temp-buf)
                 (buffer-local-value 'major-mode orig-buf))
             (not (with-current-buffer orig-buf (use-region-p))))
    (let ((prev-wire-backend (buffer-local-value 'macher-agent-wire-backend orig-buf))
          (cur-buf-backend (or (and info (plist-get info :backend))
                               (buffer-local-value 'gptel-backend orig-buf))))
      (when (and prev-wire-backend cur-buf-backend
                 (not (eq cur-buf-backend prev-wire-backend)))
        (macher-agent-wire-clear-buffer orig-buf))
      (let* ((pm (and (macher-agent-plist-p info) (plist-get info :position)))
             (send-end (if (and (markerp pm) (eq (marker-buffer pm) orig-buf))
                           (marker-position pm)
                         (with-current-buffer orig-buf (point-max))))
             (orig-prompt-beg (with-current-buffer orig-buf
                                (macher-agent-prompt-start-pos send-end)))
             (has-spans (macher-agent-wire-has-spans-p orig-buf))
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
                     (not (buffer-local-value 'macher-agent-wire-backend orig-buf)))
            (with-current-buffer orig-buf
              (setq-local macher-agent-wire-backend cur-buf-backend)))
          (macher-agent-wire-sync-spans
           orig-buf orig-prompt-beg
           (or (buffer-local-value 'macher-agent-wire-backend orig-buf)
               cur-buf-backend)))))))

(defun macher-agent-wire--on-prepare-prompt (active-fsm temp-buf orig-buf state payload prompt-start-marker)
  "Reconcile shadow wire prefix and trim TEMP-BUF history for ACTIVE-FSM.
Uses ORIG-BUF, transmission STATE, PAYLOAD, and PROMPT-START-MARKER."
  (let* ((cur-info (and active-fsm (gptel-fsm-info active-fsm)))
         (wire-ok (and active-fsm
                       (not (eq temp-buf orig-buf))
                       (buffer-live-p orig-buf)
                       (macher-agent-wire--enabled-p orig-buf cur-info)
                       (eq (buffer-local-value 'major-mode temp-buf)
                           (buffer-local-value 'major-mode orig-buf))
                       (not (with-current-buffer orig-buf (use-region-p)))))
         (_ (when wire-ok
              (let ((eff-backend (plist-get payload :backend))
                    (prev-wire-backend (buffer-local-value 'macher-agent-wire-backend orig-buf)))
                (when (and prev-wire-backend
                           (not (eq eff-backend prev-wire-backend)))
                  (macher-agent-wire-clear-buffer orig-buf))
                (when (or (not (plist-get cur-info :dry-run))
                          (eq eff-backend (buffer-local-value 'gptel-backend orig-buf)))
                  (with-current-buffer orig-buf
                    (setq-local macher-agent-wire-backend eff-backend))))))
         (pos-m (and (macher-agent-plist-p cur-info) (plist-get cur-info :position)))
         (send-end (if (and (markerp pos-m) (eq (marker-buffer pos-m) orig-buf))
                       (marker-position pos-m)
                     (with-current-buffer orig-buf (point-max))))
         (orig-prompt-beg (when wire-ok
                            (with-current-buffer orig-buf
                              (macher-agent-prompt-start-pos send-end))))
         (has-wire-spans (and wire-ok (macher-agent-wire-has-spans-p orig-buf)))
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
                 (macher-agent-wire--reconcile-history
                  orig-buf
                  orig-prompt-beg
                  (plist-get payload :backend)
                  (macher-agent-transmission-state-disable-local-tools state)
                  (with-current-buffer orig-buf
                    (bound-and-true-p gptel--num-messages-to-send))
                  (macher-agent-get-max-context-chars
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
                (let ((prev-mh (buffer-local-value 'macher-agent-wire--last-media-hash orig-buf))
                      (cur-mh (when-let* ((ml (ignore-errors (gptel-context--collect-media))))
                                (sxhash-equal ml))))
                  (when (and prev-mh cur-mh (eql prev-mh cur-mh))
                    (setq-local gptel-context nil)))))
            (setq upd-info (plist-put upd-info :macher-agent-wire-prefix wire-prefix))))
         ((> ps-pos (point-min))
          (save-restriction
            (narrow-to-region (point-min) ps-pos)
            (macher-agent-extract-inline-skills (point-min) orig-buf))))
        (macher-agent-set-fsm-info active-fsm upd-info)
        (setf (gptel-fsm-info active-fsm) upd-info)))))

;;; Installation and Uninstallation

;;;###autoload
(defun macher-agent-wire-install ()
  "Install shadow wire buffer protocol bindings and hooks into `macher-agent'."
  (interactive)
  (setq macher-agent-history-chars-function #'macher-agent-wire-history-chars)
  (setq macher-agent-conversation-source-buffer-function #'macher-agent-wire-source-buffer)
  (setq macher-agent-horizon-bounds-function #'macher-agent-wire-horizon-bounds)
  (setq macher-agent-has-prior-response-function #'macher-agent-wire-has-prior-response-p)
  (setq macher-agent-last-committed-pos-function #'macher-agent-wire-last-committed-pos)
  (setq macher-agent-strip-wire-vars-function #'macher-agent-wire-strip-local-variables)
  (setq macher-agent-rebuild-prompts-function #'macher-agent-wire-rebuild-prompts)
  (add-hook 'macher-agent-fsm-hijack-hook #'macher-agent-wire--on-fsm-hijack)
  (add-hook 'macher-agent-pre-prompt-compile-hook #'macher-agent-wire--on-pre-prompt-compile)
  (add-hook 'macher-agent-prepare-wire-prompt-hook #'macher-agent-wire--on-prepare-prompt)
  (add-hook 'macher-agent-post-prompt-transform-hook #'macher-agent-wire--apply-prefix-to-fsm)
  (add-hook 'macher-agent-branch-chat-hook #'macher-agent-wire-clone-buffer)
  (add-hook 'macher-agent-backend-changed-hook #'macher-agent-wire-clear-buffer)
  (add-hook 'gptel-prompt-transform-functions #'macher-agent-wire-finalize-transform 90))

;;;###autoload
(defun macher-agent-wire-uninstall ()
  "Remove shadow wire buffer hooks and restore default `macher-agent' protocol."
  (interactive)
  (remove-hook 'gptel-prompt-transform-functions #'macher-agent-wire-finalize-transform)
  (remove-hook 'macher-agent-fsm-hijack-hook #'macher-agent-wire--on-fsm-hijack)
  (remove-hook 'macher-agent-pre-prompt-compile-hook #'macher-agent-wire--on-pre-prompt-compile)
  (remove-hook 'macher-agent-prepare-wire-prompt-hook #'macher-agent-wire--on-prepare-prompt)
  (remove-hook 'macher-agent-post-prompt-transform-hook #'macher-agent-wire--apply-prefix-to-fsm)
  (remove-hook 'macher-agent-branch-chat-hook #'macher-agent-wire-clone-buffer)
  (remove-hook 'macher-agent-backend-changed-hook #'macher-agent-wire-clear-buffer)
  (when (eq macher-agent-history-chars-function #'macher-agent-wire-history-chars)
    (setq macher-agent-history-chars-function #'macher-agent-default-history-chars))
  (when (eq macher-agent-conversation-source-buffer-function #'macher-agent-wire-source-buffer)
    (setq macher-agent-conversation-source-buffer-function #'identity))
  (when (eq macher-agent-horizon-bounds-function #'macher-agent-wire-horizon-bounds)
    (setq macher-agent-horizon-bounds-function nil))
  (when (eq macher-agent-has-prior-response-function #'macher-agent-wire-has-prior-response-p)
    (setq macher-agent-has-prior-response-function nil))
  (when (eq macher-agent-last-committed-pos-function #'macher-agent-wire-last-committed-pos)
    (setq macher-agent-last-committed-pos-function nil))
  (when (eq macher-agent-strip-wire-vars-function #'macher-agent-wire-strip-local-variables)
    (setq macher-agent-strip-wire-vars-function nil))
  (when (eq macher-agent-rebuild-prompts-function #'macher-agent-wire-rebuild-prompts)
    (setq macher-agent-rebuild-prompts-function nil)))

(add-hook 'macher-agent-install-hook #'macher-agent-wire-install)
(add-hook 'macher-agent-uninstall-hook #'macher-agent-wire-uninstall)
(when (and (boundp 'gptel-prompt-transform-functions)
           (memq #'macher-agent-fsm-hijack-transform
                 (default-value 'gptel-prompt-transform-functions)))
  (macher-agent-wire-install))

(provide 'macher-agent-wire)
;;; macher-agent-wire.el ends here
