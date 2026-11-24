;;; test-wire.el --- Shadow wire buffer and inter-turn KV cache invariants -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Specification 10: Shadow wire buffer, thoughtSignature preservation,
;; user-buffer edit reconciliation, and over-the-wire Turn 1 -> Turn 2 validation.
;; WIR-01, WIR-02, WIR-03, WIR-04, WIR-05, WIR-06

;;; Code:

(unless (featurep 'macher-agent-wire-test-setup)
  (load (expand-file-name "macher-agent-wire-test-setup.el"
                          (file-name-directory (or load-file-name buffer-file-name)))
        nil t))

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'buttercup)
(require 'macher-agent-test-helpers)
(require 'macher-agent-wire)
(require 'gptel-gemini)
(require 'gptel-anthropic)
(require 'gptel-openai)
(require 'gptel-openai-responses)
(require 'gptel-org)

(cl-defun macher-agent-test--simulate-gemini-tool-turn (buf backend &key
                                                            (steps 9)
                                                            (stream t)
                                                            (final-text "Completed architectural audit of macher-agent-core.el.")
                                                            (pre-tool-text nil)
                                                            (tool-call-fn nil))
  "Drive a realistic Gemini FSM turn on BUF with BACKEND across STEPS tool calls.
Simulates `INIT -> WAIT -> (TYPE -> TPRE -> TOOL -> TRET -> WAIT)* -> TYPE -> DONE'
using `gptel--parse-response' and `gptel--handle-tool-result' with `gptel-log-level'
bound to nil.  Optional TOOL-CALL-FN receives IDX and returns `(:name ... :args ...)'.
Returns a plist with `:step-snapshot', `:turn1-contents', and `:fsm'."
  (with-current-buffer buf
    (goto-char (point-max))
    (let* ((gptel-backend backend)
           (gptel-model 'gemini-3.1-pro-preview)
           (gptel-stream stream)
           (gptel-use-curl t)
           (gptel-log-level nil)
           (fsm (gptel-make-fsm :table (copy-alist gptel-send--transitions)
                                :handlers (copy-alist gptel-send--handlers)))
           (step-9-snapshot nil))
      (cl-letf (((symbol-function 'gptel-curl-get-response)
                 (lambda (f)
                   (let* ((inf (gptel-fsm-info f))
                          (st (plist-get inf :stream)))
                     (unless (plist-get inf :uuid)
                       (setf (gptel-fsm-info f)
                             (nconc (list :uuid "sim-uuid"
                                          :transformer
                                          (when (with-current-buffer (plist-get inf :buffer)
                                                  (and (derived-mode-p 'org-mode)
                                                       gptel-org-convert-response))
                                            (gptel--stream-convert-markdown->org
                                             (plist-get inf :position))))
                                    (unless (plist-get inf :callback)
                                      (list :callback (if st
                                                          #'gptel-curl--stream-insert-response
                                                        #'gptel--insert-response)))
                                    inf))))))
                ((symbol-function 'gptel--url-get-response) #'ignore))
        ;; Run INIT ->gptel-prompt-transform-functions -> gptel--realize-query -> WAIT
        (gptel-request nil
                       :stream stream
                       :transforms gptel-prompt-transform-functions
                       :fsm fsm)
        (expect (gptel-fsm-state fsm) :to-equal 'WAIT)
        ;; Drive STEPS tool iterations
        (dotimes (idx steps)
          (let* ((step-num (1+ idx))
                 (call-id (format "call_%d" (+ 3712240795627520620 step-num)))
                 (sig (format "Ev8ICvwIAWkUfRM7dMrKIKpjVhFOv0Amrg_STEP_%02d_%s=="
                              step-num (make-string 48 ?A)))
                 (thought-txt (format "**Inspecting Step %d**\n\nAnalysing directives and state slice %d...\n\n"
                                      step-num step-num))
                 (resp-payload
                  (list :candidates
                        (vector
                         (list :content
                               (list :role "model"
                                     :parts
                                     (vconcat
                                      (list (list :text thought-txt :thought t))
                                      (when (and (= idx 0) pre-tool-text)
                                        (list (list :text pre-tool-text)))
                                      (list (list :functionCall
                                                  (if tool-call-fn
                                                      (funcall tool-call-fn idx)
                                                    (list :name "wire_read_tool"
                                                          :args (list :path "macher-agent-core.el"
                                                                      :offset (* idx 60)
                                                                      :limit 60
                                                                      :show_line_numbers t)))
                                                  :id call-id
                                                  :thoughtSignature sig)
                                            (list :text ""))))
                               :finishReason "STOP"))))
                 (inf (gptel-fsm-info fsm))
                 (cb (plist-get inf :callback)))
            ;; WAIT -> TYPE
            (plist-put inf :http-status "200")
            (plist-put inf :status "HTTP/2 200")
            (gptel--fsm-transition fsm)
            (expect (gptel-fsm-state fsm) :to-equal 'TYPE)
            (let ((parsed-txt (gptel--parse-response
                               backend resp-payload inf (when stream 'include))))
              (when (plist-get inf :reasoning)
                (funcall cb (cons 'reasoning (plist-get inf :reasoning)) inf)
                (plist-put inf :reasoning nil)
                (when stream
                  (funcall cb '(reasoning . t) inf)
                  (plist-put inf :reasoning-block 'done)))
              (when (and parsed-txt (not (string-empty-p parsed-txt)))
                (funcall cb parsed-txt inf)))
            ;; Snapshot wire :data on the final intra-turn tool step
            ;; Execute TPRE -> TOOL -> TRET -> WAIT synchronously via tool function
            (gptel--fsm-transition fsm)
            (expect (gptel-fsm-state fsm) :to-equal 'WAIT)
            (when (= step-num steps)
              (setq step-9-snapshot
                    (macher-agent-wire-deep-copy-data
                     (plist-get (gptel-fsm-info fsm) :data))))))
        ;; Final response step: WAIT -> TYPE -> DONE
        (let* ((final-sig (concat "FinalSig_Ev8ICvwIAWkUfRM7dMrK_" (make-string 56 ?Z) "=="))
               (final-thought "**Synthesising Findings**\n\nAll slices verified; preparing concise summary.\n\n")
               (final-payload
                (list :candidates
                      (vector
                       (list :content
                             (list :role "model"
                                   :parts
                                   (vconcat
                                    (list (list :text final-thought :thought t))
                                    (if (and final-text (not (string-empty-p final-text)))
                                        (list (list :text final-text :thoughtSignature final-sig))
                                      (list (list :text "" :thoughtSignature final-sig)))))
                             :finishReason "STOP"))))
               (inf (gptel-fsm-info fsm))
               (cb (plist-get inf :callback)))
          (plist-put inf :http-status "200")
          (plist-put inf :status "HTTP/2 200")
          (gptel--fsm-transition fsm)
          (expect (gptel-fsm-state fsm) :to-equal 'TYPE)
          (let ((parsed-txt (gptel--parse-response
                             backend final-payload inf (when stream 'include))))
            (when (plist-get inf :reasoning)
              (funcall cb (cons 'reasoning (plist-get inf :reasoning)) inf)
              (plist-put inf :reasoning nil)
              (when stream
                (funcall cb '(reasoning . t) inf)
                (plist-put inf :reasoning-block 'done)))
            (when (and parsed-txt (not (string-empty-p parsed-txt)))
              (funcall cb parsed-txt inf))
            (when stream
              (funcall cb t inf)))
          ;; Capture complete Turn 1 :contents after macher-agent--capture-wire-done
          ;; and before gptel--fsm-last clears :data when gptel-log-level is nil
          (let ((complete-turn1-contents nil))
            (let* ((hs (copy-alist (gptel-fsm-handlers fsm)))
                   (done-hs (alist-get 'DONE hs))
                   (probe (lambda (f)
                            (setq complete-turn1-contents
                                  (macher-agent-wire-deep-copy-data
                                   (plist-get (plist-get (gptel-fsm-info f) :data) :contents))))))
              (setf (alist-get 'DONE hs)
                    (cl-mapcan (lambda (h)
                                 (if (eq h #'gptel--fsm-last)
                                     (list probe h)
                                   (list h)))
                               done-hs))
              (setf (gptel-fsm-handlers fsm) hs))
            (gptel--fsm-transition fsm)
            (expect (gptel-fsm-state fsm) :to-equal 'DONE)
            ;; Verify gptel--fsm-last actually cleared :contents on the finished FSM because gptel-log-level is nil
            (expect (plist-get (plist-get (gptel-fsm-info fsm) :data) :contents) :to-be nil)
            (list :step-snapshot step-9-snapshot
                  :turn1-contents complete-turn1-contents
                  :fsm fsm)))))))

(defun macher-agent-test--install-wire-ro-preset ()
  "Register local `wire_read_tool' and `wire-ro' preset without macher/VFS."
  (let ((rt (gptel-make-tool
             :name "wire_read_tool"
             :description "Read core slice"
             :category "perception"
             :include nil
             :args '((:name "path" :type "string")
                     (:name "offset" :type "integer" :optional t)
                     (:name "limit" :type "integer" :optional t)
                     (:name "show_line_numbers" :type "boolean" :optional t))
             :function (lambda (path &optional _offset _limit _show)
                         (if (file-exists-p path)
                             (with-temp-buffer (insert-file-contents path) (buffer-string))
                           "ok")))))
    (macher-agent-register-tool-in-gptel-tables rt "perception")
    (unless (local-variable-p 'gptel--known-presets)
      (setq-local gptel--known-presets (copy-alist (bound-and-true-p gptel--known-presets))))
    (setf (alist-get 'wire-ro gptel--known-presets)
          '(:system "You are a read-only code inspector." :tools ("wire_read_tool")))))

(describe "10. Shadow wire buffer and inter-turn KV cache invariants"

  (before-each
    (macher-agent-test-reset-env))

  ;; --------------------------------------------------------------------------
  ;; WIR-01
  ;; --------------------------------------------------------------------------
  (it "WIR-01: preserves Gemini thoughtSignature, thought:true, tool IDs, and KV prefix across turns in streaming and non-streaming modes"
    (macher-agent-test-with-git-workspace (_root ctx owner-buf
                                                 `(("macher-agent-core.el" . ,(make-string 4800 ?x))))
      (let* ((gptel--known-backends nil)
             (orig-presets (copy-alist (default-value 'gptel--known-presets)))
             (orig-tools (copy-tree (default-value 'gptel--known-tools)))
             (gemini-stream (gptel-make-gemini "WireGeminiStream"
                              :models '(gemini-3.1-pro-preview)
                              :stream t))
             (gemini-batch (gptel-make-gemini "WireGeminiBatch"
                             :models '(gemini-3.1-pro-preview)
                             :stream nil)))
        (unwind-protect
            (progn
              (dolist (mode-spec `((,gemini-stream t) (,gemini-batch nil)))
                (pcase-let ((`(,backend ,stream-flag) mode-spec))
                  (let ((chat-buf (generate-new-buffer
                                   (if stream-flag "*wir01-stream*" "*wir01-batch*"))))
                    (with-current-buffer chat-buf
                      (setq-local default-directory (macher-agent-context-project-root ctx))
                      (markdown-mode)
                      (gptel-mode 1)
                      (macher-agent-mode 1)
                      (setq-local gptel-backend backend)
                      (setq-local gptel-model 'gemini-3.1-pro-preview)
                      (setq-local gptel-stream stream-flag)
                      (macher-agent-initialize-skills ctx)
                      (macher-agent-test--install-wire-ro-preset)
                      (insert "### @wire-ro inspect core directives\n\n")
                      (let* ((res (macher-agent-test--simulate-gemini-tool-turn
                                   chat-buf backend :steps 9 :stream stream-flag))
                             (turn1-contents (plist-get res :turn1-contents)))
                        ;; Turn 1 produced 1 user + 18 tool-loop + 1 final = 20 entries
                        (expect (length turn1-contents) :to-equal 20)

                        ;; Turn 2 user prompt in chat-buf
                        (goto-char (point-max))
                        (insert "@wire-ro summarise session state\n\n")
                        (goto-char (point-max))

                        ;; Inspect Turn 2 wire payload via dry-run AND real INIT->WAIT
                        (let* ((dry-fsm (gptel-request nil
                                          :dry-run t
                                          :stream stream-flag
                                          :transforms gptel-prompt-transform-functions))
                               (dry-contents (plist-get (plist-get (gptel-fsm-info dry-fsm) :data)
                                                        :contents)))
                          (expect (length dry-contents) :to-equal 21)
                          ;; First 20 entries on Turn 2 MUST be byte-for-byte identical to Turn 1
                          (expect (seq-subseq dry-contents 0 20) :to-equal turn1-contents)
                          ;; Turn 1 user message must NOT contain "@wire-ro"
                          (expect (plist-get (aref (plist-get (aref dry-contents 0) :parts) 0) :text)
                                  :to-equal "inspect core directives")
                          ;; All intra-turn tool signatures/thoughts preserved (10 with streaming final, 9 in batch)
                          (let ((sig-count 0)
                                (thought-count 0)
                                (downgraded-thoughts 0)
                                (expected-count (if stream-flag 10 9)))
                            (dotimes (i 20)
                              (let ((parts (plist-get (aref dry-contents i) :parts)))
                                (cl-loop for p across parts do
                                         (when (plist-get p :thoughtSignature)
                                           (cl-incf sig-count))
                                         (when (plist-get p :thought)
                                           (cl-incf thought-count))
                                         (when (and (stringp (plist-get p :text))
                                                    (string-match-p "\\*\\*\\(Inspecting\\|Synthesising\\)"
                                                                    (plist-get p :text))
                                                    (not (plist-get p :thought)))
                                           (cl-incf downgraded-thoughts)))))
                            (expect sig-count :to-equal expected-count)
                            (expect thought-count :to-equal expected-count)
                            (expect downgraded-thoughts :to-equal 0)))))))))
          (set-default 'gptel--known-presets orig-presets)
          (set-default 'gptel--known-tools orig-tools)))))

  ;; --------------------------------------------------------------------------
  ;; WIR-02
  ;; --------------------------------------------------------------------------
  (it "WIR-02: keeps user chat buffer clean of thinking and tool blocks while maintaining fundamental-mode shadow buffer and cleanup"
    (macher-agent-test-with-git-workspace (_root ctx owner-buf
                                                 `(("macher-agent-core.el" . ,(make-string 6000 ?y))))
      (let* ((gptel--known-backends nil)
             (orig-presets (copy-alist (default-value 'gptel--known-presets)))
             (orig-tools (copy-tree (default-value 'gptel--known-tools)))
             (gemini (gptel-make-gemini "WireGemini02" :models '(gemini-3.1-pro-preview) :stream t)))
        (unwind-protect
            (progn
              (let ((chat-buf (generate-new-buffer "*wir02-clean*")))
                (with-current-buffer chat-buf
                  (setq-local default-directory (macher-agent-context-project-root ctx))
                  (markdown-mode)
                  (gptel-mode 1)
                  (macher-agent-mode 1)
                  ;; Fresh `macher-agent-mode' buffer leaves `gptel' display defaults untouched
                  (expect (local-variable-p 'gptel-include-reasoning) :to-be nil)
                  (expect (local-variable-p 'gptel-include-tool-results) :to-be nil)
                  (setq-local gptel-backend gemini)
                  (setq-local gptel-model 'gemini-3.1-pro-preview)
                  (setq-local gptel-include-reasoning nil)
                  (setq-local gptel-include-tool-results nil)
                  (macher-agent-initialize-skills ctx)
                  (macher-agent-test--install-wire-ro-preset)
                  (insert "### @wire-ro inspect core\n\n")
                  ;; Even when `gptel-include-reasoning' is nil in the user buffer,
                  ;; `macher-agent-sync-prompt-transformer' requests thoughts on the wire
                  ;; while keeping the user buffer's local setting nil
                  (let* ((probe-fsm (gptel-request nil
                                      :dry-run t
                                      :stream t
                                      :transforms gptel-prompt-transform-functions))
                         (probe-info (gptel-fsm-info probe-fsm))
                         (probe-gen (plist-get (plist-get probe-info :data) :generationConfig)))
                    (expect (plist-get probe-info :include-reasoning) :to-be t)
                    (expect (plist-get (plist-get probe-gen :thinkingConfig) :includeThoughts) :to-be t)
                    (expect (buffer-local-value 'gptel-include-reasoning chat-buf) :to-be nil))
                  (macher-agent-test--simulate-gemini-tool-turn
                   chat-buf gemini :steps 4 :stream t
                   :final-text "Short visible summary.")
                  (let ((wbuf (macher-agent-wire-buffer-for chat-buf nil))
                        (user-text (buffer-string)))
                    (expect (buffer-live-p wbuf) :to-be t)
                    (expect (string-prefix-p " *macher-agent-wire:" (buffer-name wbuf)) :to-be t)
                    ;; Shadow buffer performance invariants
                    (with-current-buffer wbuf
                      (expect major-mode :to-equal 'fundamental-mode)
                      (expect buffer-undo-list :to-be t)
                      (expect font-lock-mode :to-be nil)
                      (expect jit-lock-mode :to-be nil)
                      (expect bidi-display-reordering :to-be nil)
                      ;; Shadow buffer holds verbose thinking and tool outputs
                      (expect (buffer-string) :to-match "``` reasoning")
                      (expect (buffer-string) :to-match "Inspecting Step 1")
                      (expect (buffer-string) :to-match "``` tool")
                      (expect (buffer-string) :to-match "wire_read_tool"))
                    ;; User buffer stays clean and tiny
                    (expect user-text :to-match "Short visible summary\\.")
                    (expect user-text :not :to-match "Inspecting Step 1")
                    (expect user-text :not :to-match "``` reasoning")
                    (expect user-text :not :to-match "``` tool")
                    (expect (< (length user-text) 200) :to-be t)
                    (expect (> (with-current-buffer wbuf (buffer-size)) 2000) :to-be t))
                  ;; Killing chat-buf kills its shadow wire buffer automatically
                  (let ((wbuf (macher-agent-wire-buffer-for chat-buf nil)))
                    (kill-buffer chat-buf)
                    (expect (buffer-live-p wbuf) :to-be nil))))
              ;; Verify boolean `macher-agent-wire-buffer-mode' = t delegates user-buffer display
              ;; directly to `gptel-include-tool-results' (`call', `auto', `t') and
              ;; `gptel-include-reasoning' while replaying full wire entries across turns
              (let* ((macher-agent-wire-buffer-mode t)
                     (inc-tool (gptel-make-tool
                                :name "wir_included_probe"
                                :description "Included probe tool returning large output"
                                :function (lambda () (make-string 6000 ?z))
                                :include t
                                :category "test"))
                     (mix-call-fn
                      (lambda (idx)
                        (if (= idx 0)
                            (list :name "wire_read_tool"
                                  :args (list :path "macher-agent-core.el"
                                              :offset 1 :limit 60 :show_line_numbers t))
                          (list :name "wir_included_probe" :args nil)))))
                (setf (alist-get "wir_included_probe"
                                 (alist-get "test" gptel--known-tools nil nil #'equal)
                                 nil nil #'equal)
                      inc-tool)
                (puthash "wir_included_probe" inc-tool
                         (macher-agent-workspace-tools-registry ctx))
                ;; 1. `call': renders tool call headers + placeholder for all tools without full bodies
                ;; or reasoning, while Turn 2 replays full 6000-char wire results and thoughtSignatures
                (let ((call-buf (generate-new-buffer "*wir02-call*")))
                  (unwind-protect
                      (with-current-buffer call-buf
                        (setq-local default-directory (macher-agent-context-project-root ctx))
                        (markdown-mode)
                        (gptel-mode 1)
                        (macher-agent-mode 1)
                        (setq-local gptel-backend gemini)
                        (setq-local gptel-model 'gemini-3.1-pro-preview)
                        (setq-local gptel-include-reasoning nil)
                        (setq-local gptel-include-tool-results 'call)
                        (macher-agent-initialize-skills ctx)
                        (macher-agent-test--install-wire-ro-preset)
                        (setf (alist-get 'wir-compact-mix gptel--known-presets)
                              '(:system "Compact mix skill"
                                :tools ("wire_read_tool" "wir_included_probe")))
                        (insert "### @wir-compact-mix call mode check\n\n")
                        (let* ((res-c (macher-agent-test--simulate-gemini-tool-turn
                                       call-buf gemini :steps 2 :stream t
                                       :final-text "Call mode summary."
                                       :tool-call-fn mix-call-fn))
                               (t1-c (plist-get res-c :turn1-contents))
                               (u-txt (buffer-string)))
                          (expect u-txt :to-match "``` tool (wire_read_tool")
                          (expect u-txt :to-match "``` tool (wir_included_probe")
                          (expect u-txt :to-match
                                  "(Cached tool result — available during original generation but not replayed)")
                          (expect u-txt :not :to-match "Inspecting Step 1")
                          (expect u-txt :not :to-match "yyyyyyyyyyyy")
                          (expect u-txt :not :to-match "zzzzzzzzzzzz")
                          ;; Turn 2 replays full 6000-char tool results and thoughtSignatures for both tools
                          (goto-char (point-max))
                          (insert "follow-up in call mode\n\n")
                          (goto-char (point-max))
                          (let* ((fsm-c2 (gptel-request nil :dry-run t :stream t
                                                        :transforms gptel-prompt-transform-functions))
                                 (c-c2 (plist-get (plist-get (gptel-fsm-info fsm-c2) :data) :contents))
                                 (wire-json (gptel--json-encode c-c2)))
                            (expect (seq-subseq c-c2 0 (length t1-c)) :to-equal t1-c)
                            (expect wire-json :to-match "yyyyyyyyyyyy")
                            (expect wire-json :to-match "zzzzzzzzzzzz"))))
                    (when (buffer-live-p call-buf) (kill-buffer call-buf))))
                ;; 2. `auto': respects per-tool `:include' rules (`wire_read_tool' omitted,
                ;; `wir_included_probe' shown) while replaying full wire results for both
                (let ((auto-buf (generate-new-buffer "*wir02-auto*")))
                  (unwind-protect
                      (with-current-buffer auto-buf
                        (setq-local default-directory (macher-agent-context-project-root ctx))
                        (markdown-mode)
                        (gptel-mode 1)
                        (macher-agent-mode 1)
                        (setq-local gptel-backend gemini)
                        (setq-local gptel-model 'gemini-3.1-pro-preview)
                        (setq-local gptel-include-reasoning nil)
                        (setq-local gptel-include-tool-results 'auto)
                        (macher-agent-initialize-skills ctx)
                        (macher-agent-test--install-wire-ro-preset)
                        (setf (alist-get 'wir-compact-mix gptel--known-presets)
                              '(:system "Compact mix skill"
                                :tools ("wire_read_tool" "wir_included_probe")))
                        (insert "### @wir-compact-mix auto mode check\n\n")
                        (let* ((res-a (macher-agent-test--simulate-gemini-tool-turn
                                       auto-buf gemini :steps 2 :stream t
                                       :final-text "Auto mode summary."
                                       :tool-call-fn mix-call-fn))
                               (t1-a (plist-get res-a :turn1-contents))
                               (u-txt (buffer-string)))
                          (expect u-txt :not :to-match "wire_read_tool")
                          (expect u-txt :to-match "``` tool (wir_included_probe")
                          (expect u-txt :to-match "zzzzzzzzzzzz")
                          (expect u-txt :not :to-match "yyyyyyyyyyyy")
                          (expect u-txt :not :to-match "Inspecting Step 1")
                          (goto-char (point-max))
                          (insert "follow-up in auto mode\n\n")
                          (goto-char (point-max))
                          (let* ((fsm-a2 (gptel-request nil :dry-run t :stream t
                                                        :transforms gptel-prompt-transform-functions))
                                 (c-a2 (plist-get (plist-get (gptel-fsm-info fsm-a2) :data) :contents))
                                 (wire-json (gptel--json-encode c-a2)))
                            (expect (seq-subseq c-a2 0 (length t1-a)) :to-equal t1-a)
                            (expect wire-json :to-match "yyyyyyyyyyyy")
                            (expect wire-json :to-match "zzzzzzzzzzzz"))))
                    (when (buffer-live-p auto-buf) (kill-buffer auto-buf))))
                ;; 3. Mirror (`gptel-include-tool-results' = t and `gptel-include-reasoning' = t):
                ;; renders full reasoning and all tool outputs while replaying exact wire entries
                (let ((full-buf (generate-new-buffer "*wir02-mirror*")))
                  (unwind-protect
                      (with-current-buffer full-buf
                        (setq-local default-directory (macher-agent-context-project-root ctx))
                        (markdown-mode)
                        (gptel-mode 1)
                        (macher-agent-mode 1)
                        (setq-local gptel-backend gemini)
                        (setq-local gptel-model 'gemini-3.1-pro-preview)
                        (setq-local gptel-include-reasoning t)
                        (setq-local gptel-include-tool-results t)
                        (macher-agent-initialize-skills ctx)
                        (macher-agent-test--install-wire-ro-preset)
                        (setf (alist-get 'wir-compact-mix gptel--known-presets)
                              '(:system "Compact mix skill"
                                :tools ("wire_read_tool" "wir_included_probe")))
                        (insert "### @wir-compact-mix mirror full check\n\n")
                        (macher-agent-test--simulate-gemini-tool-turn
                         full-buf gemini :steps 2 :stream t
                         :final-text "Mirror full summary."
                         :tool-call-fn mix-call-fn)
                        (let ((u-txt (buffer-string)))
                          (expect u-txt :to-match "``` reasoning")
                          (expect u-txt :to-match "Inspecting Step 1")
                          (expect u-txt :to-match "yyyyyyyyyyyy")
                          (expect u-txt :to-match "zzzzzzzzzzzz")))
                    (when (buffer-live-p full-buf) (kill-buffer full-buf))))
                ;; 4. Upstream `gptel' default (`gptel-include-reasoning' = 'ignore):
                ;; displays folded reasoning block marked `'gptel 'ignore' in user buffer
                ;; while preserving raw `thought: true' and `thoughtSignature' on Turn 2 wire
                (let ((ign-buf (generate-new-buffer "*wir02-ignore*")))
                  (unwind-protect
                      (with-current-buffer ign-buf
                        (setq-local default-directory (macher-agent-context-project-root ctx))
                        (markdown-mode)
                        (gptel-mode 1)
                        (macher-agent-mode 1)
                        (setq-local gptel-backend gemini)
                        (setq-local gptel-model 'gemini-3.1-pro-preview)
                        (setq-local gptel-include-reasoning 'ignore)
                        (setq-local gptel-include-tool-results nil)
                        (macher-agent-initialize-skills ctx)
                        (macher-agent-test--install-wire-ro-preset)
                        (insert "### @wire-ro inspect with ignore reasoning\n\n")
                        (let* ((res-i (macher-agent-test--simulate-gemini-tool-turn
                                       ign-buf gemini :steps 1 :stream t
                                       :final-text "Ignore reasoning summary."))
                               (t1-i (plist-get res-i :turn1-contents))
                               (u-txt (buffer-string)))
                          (expect u-txt :to-match "``` reasoning")
                          (expect u-txt :to-match "Inspecting Step 1")
                          (goto-char (point-max))
                          (insert "follow-up after ignore reasoning\n\n")
                          (goto-char (point-max))
                          (let* ((fsm-i2 (gptel-request nil :dry-run t :stream t
                                                        :transforms gptel-prompt-transform-functions))
                                 (c-i2 (plist-get (plist-get (gptel-fsm-info fsm-i2) :data) :contents)))
                            (expect (seq-subseq c-i2 0 (length t1-i)) :to-equal t1-i))))
                    (when (buffer-live-p ign-buf) (kill-buffer ign-buf))))))
          (set-default 'gptel--known-presets orig-presets)
          (set-default 'gptel--known-tools orig-tools)))))

  ;; --------------------------------------------------------------------------
  ;; WIR-03
  ;; --------------------------------------------------------------------------
  (it "WIR-03: preserves raw Markdown wire entries and thoughtSignature in org-mode buffers with gptel-org-convert-response and cosmetic edits"
    (macher-agent-test-with-git-workspace (_root ctx _owner-buf
                                                 '(("macher-agent-core.el" . "(defun core () t)\n")))
      (let* ((gptel--known-backends nil)
             (orig-presets (copy-alist (default-value 'gptel--known-presets)))
             (orig-tools (copy-tree (default-value 'gptel--known-tools)))
             (gemini (gptel-make-gemini "WireGeminiOrg" :models '(gemini-3.1-pro-preview) :stream t))
             (org-buf (generate-new-buffer "*wir03-org*")))
        (unwind-protect
            (progn
              (with-current-buffer org-buf
                (setq-local default-directory (macher-agent-context-project-root ctx))
                (org-mode)
                (setq-local gptel-org-convert-response t)
                (gptel-mode 1)
                (macher-agent-mode 1)
                (setq-local gptel-backend gemini)
                (setq-local gptel-model 'gemini-3.1-pro-preview)
                (macher-agent-initialize-skills ctx)
                (macher-agent-test--install-wire-ro-preset)
                (insert "*** @wire-ro inspect core in org\n\n")
                (let* ((md-reply "Here is **bold** Markdown and `inline-code` across a long sentence that can be wrapped cleanly by fill-paragraph without altering words.")
                       (res (macher-agent-test--simulate-gemini-tool-turn
                             org-buf gemini :steps 2 :stream t :final-text md-reply))
                       (turn1-contents (plist-get res :turn1-contents)))
                  ;; Visible Org buffer converted **bold** -> *bold*
                  (expect (buffer-string) :to-match "\\*bold\\*")
                  ;; Apply cosmetic line-wrapping edit in the Org response region
                  (save-excursion
                    (goto-char (point-min))
                    (when (search-forward "*bold*" nil t)
                      (fill-paragraph)))
                  ;; Send Turn 2
                  (goto-char (point-max))
                  (insert "follow-up question in org\n\n")
                  (goto-char (point-max))
                  (let* ((fsm2 (gptel-request nil
                                 :dry-run t
                                 :stream t
                                 :transforms gptel-prompt-transform-functions))
                         (contents2 (plist-get (plist-get (gptel-fsm-info fsm2) :data) :contents)))
                    ;; Exact raw Markdown + thoughtSignature replayed despite Org conversion and fill-paragraph!
                    (expect (seq-subseq contents2 0 (length turn1-contents))
                            :to-equal turn1-contents)))

                ;; Also verify non-streaming mode (where macher-agent--ensure-final-wire-entry
                ;; constructs the final wire entry) preserves raw Markdown instead of Org text
                (erase-buffer)
                (macher-agent-wire-clear-buffer org-buf)
                (let ((gemini-batch (gptel-make-gemini "WireGeminiOrgBatch"
                                      :models '(gemini-3.1-pro-preview)
                                      :stream nil)))
                  (setq-local gptel-backend gemini-batch)
                  (setq-local gptel-stream nil)
                  (insert "*** @wire-ro batch org check\n\n")
                  (let* ((batch-md "Batch **markdown** with `code_symbol` intact.")
                         (res-b (macher-agent-test--simulate-gemini-tool-turn
                                 org-buf gemini-batch :steps 1 :stream nil :final-text batch-md))
                         (t1-b (plist-get res-b :turn1-contents))
                         (final-part (aref (plist-get (aref t1-b (1- (length t1-b))) :parts) 0)))
                    ;; Visible Org buffer converted **markdown** -> *markdown*
                    (expect (buffer-string) :to-match "\\*markdown\\*")
                    ;; Wire entry stores raw Markdown, without Org conversion or reasoning leakage
                    (expect (plist-get final-part :text) :to-equal batch-md)
                    (goto-char (point-max))
                    (insert "second batch org turn\n\n")
                    (goto-char (point-max))
                    (let* ((fsm-b2 (gptel-request nil
                                     :dry-run t
                                     :stream nil
                                     :transforms gptel-prompt-transform-functions))
                           (c-b2 (plist-get (plist-get (gptel-fsm-info fsm-b2) :data) :contents)))
                      (expect (seq-subseq c-b2 0 (length t1-b)) :to-equal t1-b))))))
          (when (buffer-live-p org-buf) (kill-buffer org-buf))
          (set-default 'gptel--known-presets orig-presets)
          (set-default 'gptel--known-tools orig-tools)))))

  ;; --------------------------------------------------------------------------
  ;; WIR-04
  ;; --------------------------------------------------------------------------
  (it "WIR-04: reconciles user prompt edits, turn deletions, zero-width tool turns, manual gaps, and ERRS/ABRT rollbacks"
    (macher-agent-test-with-git-workspace (_root ctx _owner-buf
                                                 '(("macher-agent-core.el" . "(defun core () t)\n")))
      (let* ((gptel--known-backends nil)
             (orig-presets (copy-alist (default-value 'gptel--known-presets)))
             (orig-tools (copy-tree (default-value 'gptel--known-tools)))
             (gemini (gptel-make-gemini "WireGemini04" :models '(gemini-3.1-pro-preview) :stream t))
             (chat-buf (generate-new-buffer "*wir04-edits*")))
        (unwind-protect
            (progn
              (with-current-buffer chat-buf
                (setq-local default-directory (macher-agent-context-project-root ctx))
                (markdown-mode)
                (gptel-mode 1)
                (macher-agent-mode 1)
                (setq-local gptel-backend gemini)
                (setq-local gptel-model 'gemini-3.1-pro-preview)
                (setq-local gptel-include-reasoning nil)
                (setq-local gptel-include-tool-results nil)
                (macher-agent-initialize-skills ctx)
                (macher-agent-test--install-wire-ro-preset)

                ;; Turn 1: with pre-tool text + 2 tool calls + final text
                (insert "### @wire-ro first question\n\n")
                (macher-agent-test--simulate-gemini-tool-turn
                 chat-buf gemini :steps 2 :stream t
                 :pre-tool-text "Checking first...\n"
                 :final-text "First final answer.")

                ;; Turn 2: zero-width tool-only turn (empty final text)
                (goto-char (point-max))
                (insert "@wire-ro silent tool turn\n\n")
                (macher-agent-test--simulate-gemini-tool-turn
                 chat-buf gemini :steps 1 :stream t
                 :final-text "")

                ;; 1. Verify Turn 3 sees both Turn 1 and Turn 2 (including zero-width Turn 2 tool step)
                (goto-char (point-max))
                (insert "third question\n\n")
                (goto-char (point-max))
                (let* ((fsm3 (gptel-request nil :dry-run t :stream t
                                            :transforms gptel-prompt-transform-functions))
                       (c3 (plist-get (plist-get (gptel-fsm-info fsm3) :data) :contents)))
                  ;; Turn 1 (6) + Turn 2 (1 user + 2 tool + 1 empty model-final = 4) + Turn 3 user (1) = 11
                  (expect (length c3) :to-equal 11))

                ;; 2. Delete Turn 2's prompt from chat-buf -> must prune Turn 2's hidden zero-width tool loop too!
                (save-excursion
                  (goto-char (point-min))
                  (let ((found (re-search-forward "\\(?:#+[ \t\r\n]*\\)?@wire-ro silent tool turn[\n\r\t ]*" nil t))
                        (mb (match-beginning 0))
                        (me (match-end 0)))
                    (expect found :to-be-truthy)
                    (delete-region mb me)))
                (goto-char (point-max))
                (let* ((fsm-del (gptel-request nil :dry-run t :stream t
                                               :transforms gptel-prompt-transform-functions))
                       (c-del (plist-get (plist-get (gptel-fsm-info fsm-del) :data) :contents)))
                  ;; Turn 2 completely pruned: Turn 1 (6) + Turn 3 user (1) = 7; zero orphaned functionCalls
                  (expect (length c-del) :to-equal 7))

                ;; 3a. Edit Turn 1's user prompt only -> preserves Turn 1's thoughtSignature tool loop
                (save-excursion
                  (goto-char (point-min))
                  (when (search-forward "first question" nil t)
                    (replace-match "edited first question" t t)))
                (goto-char (point-max))
                (let* ((fsm-u (gptel-request nil :dry-run t :stream t
                                             :transforms gptel-prompt-transform-functions))
                       (c-u (plist-get (plist-get (gptel-fsm-info fsm-u) :data) :contents))
                       (u1-txt (plist-get (aref (plist-get (aref c-u 0) :parts) 0) :text))
                       (step1-parts (plist-get (aref c-u 1) :parts)))
                  (expect (length c-u) :to-equal 7)
                  (expect u1-txt :to-equal "edited first question")
                  (expect (cl-some (lambda (p) (plist-get p :thoughtSignature)) step1-parts) :to-be-truthy))

                ;; 3b. Edit ONLY Turn 1's final reply while leaving pre-tool text intact:
                ;; preserves Turn 1's tool loop + thoughtSignatures and updates only f-sp
                (save-excursion
                  (goto-char (point-min))
                  (when (search-forward "First final answer." nil t)
                    (replace-match "Tweaked final answer." t t)))
                (goto-char (point-max))
                (let* ((fsm-fonly (gptel-request nil :dry-run t :stream t
                                                 :transforms gptel-prompt-transform-functions))
                       (c-fonly (plist-get (plist-get (gptel-fsm-info fsm-fonly) :data) :contents))
                       (step1-parts (plist-get (aref c-fonly 1) :parts))
                       (f1-txt (plist-get (aref (plist-get (aref c-fonly 5) :parts) 0) :text)))
                  (expect (length c-fonly) :to-equal 7)
                  (expect (cl-some (lambda (p) (plist-get p :thoughtSignature)) step1-parts) :to-be-truthy)
                  (expect f1-txt :to-equal "Tweaked final answer."))

                ;; 3c. Replace across BOTH pre-tool + final text -> prunes t-sp and updates f-sp cleanly
                (save-excursion
                  (goto-char (point-min))
                  (when (search-forward "Checking first...\nTweaked final answer." nil t)
                    (replace-match "Edited assistant conclusion." t t)))
                (goto-char (point-max))
                (let* ((fsm-ed (gptel-request nil :dry-run t :stream t
                                              :transforms gptel-prompt-transform-functions))
                       (c-ed (plist-get (plist-get (gptel-fsm-info fsm-ed) :data) :contents))
                       (u1-txt (plist-get (aref (plist-get (aref c-ed 0) :parts) 0) :text))
                       (f1-txt (plist-get (aref (plist-get (aref c-ed 1) :parts) 0) :text))
                       (u3-txt (plist-get (aref (plist-get (aref c-ed 2) :parts) 0) :text)))
                  (expect (length c-ed) :to-equal 3)
                  (expect u1-txt :to-equal "edited first question")
                  (expect f1-txt :to-equal "Edited assistant conclusion.")
                  (expect u3-txt :to-equal "third question"))

                ;; 3c-bis. Verify wbuf text and glob search reflect edits & deletions and drop pre-edit/deleted text
                (let* ((wbuf (macher-agent-wire-buffer-for chat-buf nil))
                       (wstr (with-current-buffer wbuf (buffer-string))))
                  (expect wstr :to-match "edited first question")
                  (expect wstr :to-match "Edited assistant conclusion.")
                  (expect wstr :not :to-match "Tweaked final answer.")
                  (expect wstr :not :to-match "silent tool turn")
                  (expect (macher-agent-search-glob chat-buf "Edited assistant conclusion")
                          :to-match "Edited assistant conclusion.")
                  (expect (macher-agent-search-glob chat-buf "Tweaked final answer")
                          :to-match "No matches found")
                  ;; Commit Turn 3, insert a manual gap after f3's nil-type end marker, commit Turn 4, and verify immediate sync
                  (macher-agent-test--simulate-gemini-tool-turn
                   chat-buf gemini :steps 0 :stream t :final-text "Turn three reply.")
                  (goto-char (point-max))
                  (insert "\nManual gap paragraph GAP_TOKEN_4411.\n")
                  (let ((sp (point)))
                    (insert "\n")
                    (add-text-properties sp (point) '(gptel ignore rear-nonsticky (gptel))))
                  (insert "fourth question\n\n")
                  (macher-agent-test--simulate-gemini-tool-turn
                   chat-buf gemini :steps 0 :stream t :final-text "Turn four reply.")
                  (goto-char (point-max))
                  (insert "fifth question\n\n")
                  (goto-char (point-max))
                  (let* ((fsm-gap (gptel-request nil :dry-run t :stream t
                                                 :transforms gptel-prompt-transform-functions))
                         (c-gap (plist-get (plist-get (gptel-fsm-info fsm-gap) :data) :contents))
                         (wstr-gap (with-current-buffer wbuf (buffer-string))))
                    ;; Turn 1 (2) + Turn 3 (2) + Gap (1) + Turn 4 (2) + Turn 5 user (1) = 8
                    (expect (length c-gap) :to-equal 8)
                    (expect wstr-gap :to-match "GAP_TOKEN_4411")
                    (expect (macher-agent-search-glob chat-buf "GAP_TOKEN_4411")
                            :to-match "GAP_TOKEN_4411")))

                ;; 3d. @command redirect + clean-mode tool loop: editing final reply preserves
                ;; both the expanded command prompt on u-sp AND tool thoughtSignatures on t-sp
                (erase-buffer)
                (macher-agent-wire-clear-buffer chat-buf)
                (setf (alist-get 'wir-audit-cmd gptel--known-presets)
                      '(:system "Expanded audit template for $1"
                        :is-command t
                        :tools ("wire_read_tool")))
                (insert "### @wir-audit-cmd core-target\n\n")
                (macher-agent-test--simulate-gemini-tool-turn
                 chat-buf gemini :steps 2 :stream t
                 :final-text "Original command conclusion.")
                (save-excursion
                  (goto-char (point-min))
                  (when (search-forward "Original command conclusion." nil t)
                    (replace-match "Refined command conclusion." t t)))
                (goto-char (point-max))
                (insert "follow-up after command edit\n\n")
                (goto-char (point-max))
                (let* ((fsm-cmd (gptel-request nil :dry-run t :stream t
                                               :transforms gptel-prompt-transform-functions))
                       (c-cmd (plist-get (plist-get (gptel-fsm-info fsm-cmd) :data) :contents))
                       (cmd-u1 (plist-get (aref (plist-get (aref c-cmd 0) :parts) 0) :text))
                       (cmd-s1-parts (plist-get (aref c-cmd 1) :parts))
                       (cmd-f1 (plist-get (aref (plist-get (aref c-cmd 5) :parts) 0) :text)))
                  (expect (length c-cmd) :to-equal 7)
                  (expect cmd-u1 :to-equal "Expanded audit template for core-target")
                  (expect (cl-some (lambda (p) (plist-get p :thoughtSignature)) cmd-s1-parts) :to-be-truthy)
                  (expect cmd-f1 :to-equal "Refined command conclusion."))

                ;; 4. Simulate aborted mid-turn request: uncommitted shadow text must roll back on ABRT
                (let* ((wbuf (macher-agent-wire-buffer-for chat-buf nil))
                       (size-before (with-current-buffer wbuf (buffer-size)))
                       (abrt-fsm (gptel-make-fsm :table (copy-alist gptel-send--transitions)
                                                 :handlers (copy-alist gptel-send--handlers))))
                  (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
                    (goto-char (point-max))
                    (gptel-request nil :stream t :transforms gptel-prompt-transform-functions :fsm abrt-fsm)
                    (expect (> (with-current-buffer wbuf (buffer-size)) size-before) :to-be t)
                    (gptel--fsm-transition abrt-fsm 'ABRT)
                    (expect (with-current-buffer wbuf (buffer-size)) :to-equal size-before))

                  ;; 5. Full erase-buffer + new prompt prunes all shadow spans cleanly
                  (erase-buffer)
                  (insert "fresh post-erase prompt\n\n")
                  (goto-char (point-max))
                  (let* ((fsm-er (gptel-request nil :dry-run t :stream t
                                                :transforms gptel-prompt-transform-functions))
                         (c-er (plist-get (plist-get (gptel-fsm-info fsm-er) :data) :contents)))
                    (expect (macher-agent-wire-has-spans-p chat-buf) :to-be nil)
                    (expect (length c-er) :to-equal 1)))))
          (when (buffer-live-p chat-buf) (kill-buffer chat-buf))
          (set-default 'gptel--known-presets orig-presets)
          (set-default 'gptel--known-tools orig-tools)))))

  ;; --------------------------------------------------------------------------
  ;; WIR-05
  ;; --------------------------------------------------------------------------
  (it "WIR-05: fast-fails on cross-backend switches, clones shadow wire in branch-chat, and truncates single oversized turns"
    (macher-agent-test-with-git-workspace (_root ctx _owner-buf
                                                 '(("macher-agent-core.el" . ";; SENTINEL_QUANTUM_TOKEN_98765\n(defun core () t)\n")))
      (let* ((gptel--known-backends nil)
             (orig-presets (copy-alist (default-value 'gptel--known-presets)))
             (orig-tools (copy-tree (default-value 'gptel--known-tools)))
             (gemini (gptel-make-gemini "WireGem05" :models '(gemini-3.1-pro-preview) :stream t))
             (openai (gptel-make-openai "WireOAI05" :host "localhost" :models '(gpt-4o) :stream nil))
             (chat-buf (generate-new-buffer "*wir05-main*"))
             (branch-buf nil))
        (unwind-protect
            (progn
              (with-current-buffer chat-buf
                (setq-local default-directory (macher-agent-context-project-root ctx))
                (markdown-mode)
                (gptel-mode 1)
                (macher-agent-mode 1)
                (setq-local gptel-backend gemini)
                (setq-local gptel-model 'gemini-3.1-pro-preview)
                (macher-agent-initialize-skills ctx)
                (macher-agent-test--install-wire-ro-preset)

                ;; Turn 1 reads SENTINEL_QUANTUM_TOKEN_98765 into hidden shadow buffer only
                (insert "### @wire-ro read core file\n\n")
                (macher-agent-test--simulate-gemini-tool-turn
                 chat-buf gemini :steps 1 :stream t
                 :final-text "File inspected.")
                (expect (buffer-string) :not :to-match "SENTINEL_QUANTUM_TOKEN_98765")

                ;; 1. Branch chat clones shadow wire buffer with independent markers
                (save-window-excursion
                  (setq branch-buf (macher-agent-branch-chat "*wir05-branch*")))
                (expect (macher-agent-wire-has-spans-p branch-buf) :to-be t)
                (expect (eq (macher-agent-wire-buffer-for branch-buf nil)
                            (macher-agent-wire-buffer-for chat-buf nil))
                        :to-be nil)

                ;; 2. Switching chat-buf to OpenAI Chat clears shadow wire buffer and parses orig-buf cleanly
                (setq-local gptel-backend openai)
                (setq-local gptel-model 'gpt-4o)
                (goto-char (point-max))
                (insert "continue on openai\n\n")
                (goto-char (point-max))
                (let* ((fsm-oai (gptel-request nil :dry-run t :stream nil
                                               :transforms gptel-prompt-transform-functions))
                       (msgs (macher-agent-existing-prompts-from-data
                              (plist-get (gptel-fsm-info fsm-oai) :data))))
                  (expect (macher-agent-wire-has-spans-p chat-buf) :to-be nil)
                  (expect (length msgs) :to-equal 3)
                  (expect (plist-get (nth 0 msgs) :content) :to-equal "read core file")
                  (expect (plist-get (nth 1 msgs) :content) :to-equal "File inspected.")
                  (expect (plist-get (nth 2 msgs) :content) :to-equal "continue on openai"))

                ;; 3. Branch buffer on Gemini still has untouched thoughtSignature on its cloned wire spans
                (with-current-buffer branch-buf
                  (goto-char (point-max))
                  (insert "continue on gemini branch\n\n")
                  (goto-char (point-max))
                  (let* ((fsm-br (gptel-request nil :dry-run t :stream t
                                                :transforms gptel-prompt-transform-functions))
                         (br-json (json-serialize (plist-get (gptel-fsm-info fsm-br) :data)
                                                  :null-object :null :false-object :json-false)))
                    (expect br-json :to-match "thoughtSignature")))

                ;; 4. Truncates even a single oversized prior turn and recalls hidden shadow tool output via glob search
                (setq-local gptel-backend gemini)
                (setq-local gptel-model 'gemini-3.1-pro-preview)
                (let ((macher-agent-max-context-chars '((nil . 120))))
                  (erase-buffer)
                  (macher-agent-wire-clear-buffer chat-buf)
                  (insert "### @wire-ro read sentinel\n\n")
                  (macher-agent-test--simulate-gemini-tool-turn
                   chat-buf gemini :steps 2 :stream t :final-text "Turn one done.")
                  (goto-char (point-max))
                  (insert "recall SENTINEL_QUANTUM_TOKEN_98765\n\n")
                  (goto-char (point-max))
                  (let* ((fsm-zm (gptel-request nil :dry-run t :stream t
                                                :transforms gptel-prompt-transform-functions))
                         (zm-info (gptel-fsm-info fsm-zm))
                         (zm-contents (plist-get (plist-get zm-info :data) :contents))
                         (alert-text (plist-get (aref (plist-get (aref zm-contents 0) :parts) 0) :text))
                         (hbounds (macher-agent-wire-horizon-bounds chat-buf 120))
                         (recall (macher-agent-search-dispatch
                                  "SENTINEL_QUANTUM_TOKEN_98765" chat-buf)))
                    ;; Single oversized historical turn replaced by synthetic alert + active user prompt (= 2 entries)
                    (expect (length zm-contents) :to-equal 2)
                    (expect alert-text :to-match "SYSTEM ALERT: macher-agent truncated")
                    (expect alert-text :to-match "search_conversation_history")
                    (expect macher-agent-wire-horizon-turn-id :to-equal 1)
                    (expect macher-agent-wire-horizon-lines-deleted :to-be-greater-than 0)
                    (expect (plist-get hbounds :wire-managed) :to-be t)
                    (expect (plist-get hbounds :offset) :to-be-greater-than 1)
                    (expect recall :to-match "SENTINEL_QUANTUM_TOKEN_98765")))

                ;; 5. Mid-turn macher-agent-wire-clear-buffer (such as mid-turn backend switch)
                ;; resets active FSM shadow offsets without signaling args-out-of-range at DONE
                (erase-buffer)
                (macher-agent-wire-clear-buffer chat-buf)
                (insert "### @wire-ro seed initial shadow history\n\n")
                (macher-agent-test--simulate-gemini-tool-turn
                 chat-buf gemini :steps 1 :stream t :final-text "Seeded.")
                (goto-char (point-max))
                (insert "@wire-ro mid-turn clear test\n\n")
                (goto-char (point-max))
                (let ((mid-fsm (gptel-make-fsm :table (copy-alist gptel-send--transitions)
                                               :handlers (copy-alist gptel-send--handlers))))
                  (cl-letf (((symbol-function 'gptel-curl-get-response)
                             (lambda (f)
                               (let ((inf (gptel-fsm-info f)))
                                 (unless (plist-get inf :uuid)
                                   (setf (gptel-fsm-info f)
                                         (nconc (list :uuid "mid-uuid"
                                                      :callback #'gptel-curl--stream-insert-response)
                                                inf)))))))
                    (gptel-request nil :stream t :transforms gptel-prompt-transform-functions :fsm mid-fsm)
                    (expect (gptel-fsm-state mid-fsm) :to-equal 'WAIT)
                    ;; Clear wire buffer while turn is in flight (erasing wbuf back to point-max = 1)
                    (macher-agent-wire-clear-buffer chat-buf)
                    (let* ((inf (gptel-fsm-info mid-fsm))
                           (cb (plist-get inf :callback))
                           (step-resp
                            (list :candidates
                                  (vector (list :content
                                                (list :role "model"
                                                      :parts (vector (list :functionCall
                                                                           (list :name "wire_read_tool"
                                                                                 :args (list :path "macher-agent-core.el"))
                                                                           :id "call_mid_1")))
                                                :finishReason "STOP"))))
                           (final-resp
                            (list :candidates
                                  (vector (list :content
                                                (list :role "model"
                                                      :parts (vector (list :text "Completed after mid-turn clear.")))
                                                :finishReason "STOP")))))
                      (plist-put inf :http-status "200")
                      (plist-put inf :status "HTTP/2 200")
                      (gptel--fsm-transition mid-fsm)
                      (gptel--parse-response gemini step-resp inf 'include)
                      (gptel--fsm-transition mid-fsm)
                      (expect (gptel-fsm-state mid-fsm) :to-equal 'WAIT)
                      ;; Ensure intra-turn WAIT did not re-arm partial turn capture
                      (expect (plist-get (gptel-fsm-info mid-fsm) :macher-agent-turn-started) :to-be nil)
                      (plist-put inf :http-status "200")
                      (plist-put inf :status "HTTP/2 200")
                      (gptel--fsm-transition mid-fsm)
                      (let ((txt (gptel--parse-response gemini final-resp inf 'include)))
                        (when txt (funcall cb txt inf))
                        (funcall cb t inf))
                      (gptel--fsm-transition mid-fsm)
                      (expect (gptel-fsm-state mid-fsm) :to-equal 'DONE)))
                  ;; Next fresh turn records shadow wire spans normally again
                  (goto-char (point-max))
                  (insert "@wire-ro fresh turn after clear\n\n")
                  (macher-agent-test--simulate-gemini-tool-turn
                   chat-buf gemini :steps 1 :stream t :final-text "Recovered cleanly.")
                  (expect (macher-agent-wire-has-spans-p chat-buf) :to-be t))))
          (when (buffer-live-p chat-buf) (kill-buffer chat-buf))
          (when (buffer-live-p branch-buf) (kill-buffer branch-buf))
          (set-default 'gptel--known-presets orig-presets)
          (set-default 'gptel--known-tools orig-tools)))))

  ;; --------------------------------------------------------------------------
  ;; WIR-06
  ;; --------------------------------------------------------------------------
  (it "WIR-06: validates over-the-wire Turn 2 INIT improvements against old HEAD and clean Emacs subprocesses"
    (let* ((new-dir macher-agent-test-root-dir)
           (wire-dir (file-name-directory (locate-library "macher-agent-wire")))
           (installed-lib (locate-library
                           "macher-agent" nil
                           (cl-remove-if
                            (lambda (p)
                              (or (not (stringp p))
                                  (string-empty-p p)
                                  (not (file-name-absolute-p p))
                                  (ignore-errors (file-equal-p p new-dir))))
                            load-path)))
           (installed-dir (and installed-lib (file-name-directory installed-lib)))
           (gptel-dir (file-name-directory (locate-library "gptel")))
           (emacs-bin (expand-file-name invocation-name invocation-directory))
           (sim-script (make-temp-file "macher-wire-sim-" nil ".el")))
      (unwind-protect
          (progn
            ;; Write standalone simulation script runnable identically in old installed/HEAD and new workspace
            (with-temp-file sim-script
              (insert
               ";;; macher-wire-sim.el --- Wire benchmark -*- lexical-binding: t; -*-\n"
               "(require 'cl-lib)\n"
               "(require 'json)\n"
               "(require 'subr-x)\n"
               "(require 'gptel)\n"
               "(require 'gptel-gemini)\n"
               "(require 'macher-agent)\n"
               "(unless (getenv \"WIRE_BENCH_DISABLE_SHADOW\")\n"
               "  (require 'macher-agent-wire nil t))\n"
               "(macher-agent-install)\n"
               "(when (getenv \"WIRE_BENCH_DISABLE_SHADOW\")\n"
               "  (setq macher-agent-wire-buffer-mode nil))\n"
               "(defun wire-bench-common-prefix (s1 s2)\n"
               "  (let ((len (min (length s1) (length s2))) (i 0))\n"
               "    (while (and (< i len) (eq (aref s1 i) (aref s2 i))) (setq i (1+ i)))\n"
               "    i))\n"
               "(let* ((ws-dir (file-name-as-directory (file-truename (make-temp-file \"wire-bench-ws-\" t))))\n"
               "       (core-file (expand-file-name \"macher-agent-core.el\" ws-dir))\n"
               "       (gptel--known-backends nil)\n"
               "       (backend (gptel-make-gemini \"BenchGemini\" :models '(gemini-3.1-pro-preview) :stream t))\n"
               "       (buf (generate-new-buffer \"*wire-bench-chat*\"))\n"
               "       (turn1-wire-json nil)\n"
               "       (turn2-wire-json nil))\n"
               "  (unwind-protect\n"
               "      (progn\n"
               "        (call-process \"git\" nil nil nil \"-C\" ws-dir \"init\" \"-q\")\n"
               "        (with-temp-file core-file\n"
               "          (dotimes (ln 360)\n"
               "            (insert (format \";; Line %03d: directive configuration and session state payload padding\\n\" (1+ ln)))))\n"
               "        (with-current-buffer buf\n"
               "          (setq-local default-directory ws-dir)\n"
               "          (text-mode)\n"
               "          (gptel-mode 1)\n"
               "          (macher-agent-mode 1)\n"
               "          (setq-local gptel-backend backend)\n"
               "          (setq-local gptel-model 'gemini-3.1-pro-preview)\n"
               "          (setq-local gptel-stream t)\n"
               "          (setq-local gptel-use-curl t)\n"
               "          (setq-local gptel-log-level nil)\n"
               "          (let ((rt (gptel-make-tool :name \"wire_read_tool\" :description \"Read slice\" :category \"perception\" :include nil :args '((:name \"path\" :type \"string\") (:name \"offset\" :type \"integer\" :optional t) (:name \"limit\" :type \"integer\" :optional t) (:name \"show_line_numbers\" :type \"boolean\" :optional t)) :function (lambda (p &optional _o _l _s) (with-temp-buffer (insert-file-contents p) (buffer-string))))))\n"
               "            (macher-agent-register-tool-in-gptel-tables rt \"perception\")\n"
               "            (setf (alist-get 'wire-ro gptel--known-presets) '(:system \"Read-only inspector.\" :tools (\"wire_read_tool\"))))\n"
               "          (if (getenv \"WIRE_BENCH_VERBOSE_UI\")\n"
               "              (progn\n"
               "                (setq-local gptel-include-reasoning t)\n"
               "                (setq-local gptel-include-tool-results t))\n"
               "            (setq-local gptel-include-reasoning nil)\n"
               "            (setq-local gptel-include-tool-results nil))\n"
               "          (insert \"### @wire-ro inspect current directives across macher-agent-core.el\\n\\n\")\n"
               "          (goto-char (point-max))\n"
               "          (let ((fsm1 (gptel-make-fsm :table (copy-alist gptel-send--transitions)\n"
               "                                      :handlers (copy-alist gptel-send--handlers))))\n"
               "            (cl-letf (((symbol-function 'gptel-curl-get-response)\n"
               "                       (lambda (f)\n"
               "                         (let ((inf (gptel-fsm-info f)))\n"
               "                           (unless (plist-get inf :uuid)\n"
               "                             (setf (gptel-fsm-info f)\n"
               "                                   (nconc (list :uuid \"bench-uuid\")\n"
               "                                          (unless (plist-get inf :callback)\n"
               "                                            (list :callback #'gptel-curl--stream-insert-response))\n"
               "                                          inf)))))))\n"
               "              (gptel-request nil :stream t :transforms gptel-prompt-transform-functions :fsm fsm1)\n"
               "              (dotimes (idx 9)\n"
               "                (let* ((step (1+ idx))\n"
               "                       (cid (format \"call_%d\" (+ 3712240795627520620 step)))\n"
               "                       (sig (format \"Ev8ICvwIAWkUfRM7dMrKIKpjVhFOv0Amrg_S%02d_%s==\" step (make-string 64 ?X)))\n"
               "                       (th (format \"**Inspecting Current Directives (Step %d)**\\n\\n Focusing on active runtime slice %d...\\n\\n\" step step))\n"
               "                       (resp (list :candidates\n"
               "                                   (vector (list :content\n"
               "                                                 (list :role \"model\"\n"
               "                                                       :parts (vector (list :text th :thought t)\n"
               "                                                                      (list :functionCall\n"
               "                                                                            (list :name \"wire_read_tool\"\n"
               "                                                                                  :args (list :path \"macher-agent-core.el\"\n"
               "                                                                                              :offset (1+ (* idx 40))\n"
               "                                                                                              :limit 40\n"
               "                                                                                              :show_line_numbers t))\n"
               "                                                                            :id cid\n"
               "                                                                            :thoughtSignature sig)\n"
               "                                                                      (list :text \"\")))\n"
               "                                                 :finishReason \"STOP\"))))\n"
               "                       (inf (gptel-fsm-info fsm1))\n"
               "                       (cb (plist-get inf :callback)))\n"
               "                  (plist-put inf :http-status \"200\")\n"
               "                  (plist-put inf :status \"HTTP/2 200\")\n"
               "                  (gptel--fsm-transition fsm1)\n"
               "                  (gptel--parse-response backend resp inf 'include)\n"
               "                  (when (plist-get inf :reasoning)\n"
               "                    (funcall cb (cons 'reasoning (plist-get inf :reasoning)) inf)\n"
               "                    (plist-put inf :reasoning nil)\n"
               "                    (funcall cb '(reasoning . t) inf)\n"
               "                    (plist-put inf :reasoning-block 'done))\n"
               "                  (gptel--fsm-transition fsm1)))\n"
               "              (let* ((fsig (concat \"FinalSig_\" (make-string 64 ?Y) \"==\"))\n"
               "                     (fresp (list :candidates\n"
               "                                  (vector (list :content\n"
               "                                                (list :role \"model\"\n"
               "                                                      :parts (vector (list :text \"**Finalising Plan**\\n\\nAll 9 slices read.\\n\\n\" :thought t)\n"
               "                                                                     (list :text \"Here is the initial plan based on all nine slices.\" :thoughtSignature fsig)))\n"
               "                                                :finishReason \"STOP\"))))\n"
               "                     (inf (gptel-fsm-info fsm1))\n"
               "                     (cb (plist-get inf :callback)))\n"
               "                (plist-put inf :http-status \"200\")\n"
               "                (plist-put inf :status \"HTTP/2 200\")\n"
               "                (gptel--fsm-transition fsm1)\n"
               "                (let ((txt (gptel--parse-response backend fresp inf 'include)))\n"
               "                  (when (plist-get inf :reasoning)\n"
               "                    (funcall cb (cons 'reasoning (plist-get inf :reasoning)) inf)\n"
               "                    (plist-put inf :reasoning nil)\n"
               "                    (funcall cb '(reasoning . t) inf)\n"
               "                    (plist-put inf :reasoning-block 'done))\n"
               "                  (when txt (funcall cb txt inf))\n"
               "                  (funcall cb t inf))\n"
               "                (setq turn1-wire-json\n"
               "                      (gptel--json-encode (plist-get (plist-get (gptel-fsm-info fsm1) :data) :contents)))\n"
               "                (gptel--fsm-transition fsm1)))\n"
               "            ;; Turn 2: user sends follow-up via gptel-send / INIT -> WAIT\n"
               "            (goto-char (point-max))\n"
               "            (insert \"@wire-ro confirm whether TYPE->TPRE->TOOL->TRET->WAIT stays unchanged\\n\\n\")\n"
               "            (goto-char (point-max))\n"
               "            (let ((fsm2 (gptel-make-fsm :table (copy-alist gptel-send--transitions)\n"
               "                                        :handlers (copy-alist gptel-send--handlers))))\n"
               "              (cl-letf (((symbol-function 'gptel-curl-get-response)\n"
               "                         (lambda (f)\n"
               "                           (setq turn2-wire-json\n"
               "                                 (gptel--json-encode (plist-get (plist-get (gptel-fsm-info f) :data) :contents))))))\n"
               "                (gptel-request nil :stream t :transforms gptel-prompt-transform-functions :fsm fsm2)))\n"
               "            (let* ((t1-vec (json-parse-string turn1-wire-json :object-type 'plist))\n"
               "                   (t2-vec (json-parse-string turn2-wire-json :object-type 'plist))\n"
               "                   (cp (wire-bench-common-prefix turn1-wire-json turn2-wire-json))\n"
               "                   (sigs 0) (thoughts 0) (leaked-thoughts 0) (replayed-tool-calls 0))\n"
               "              (dotimes (i (1- (length t2-vec)))\n"
               "                (let ((parts (plist-get (aref t2-vec i) :parts)))\n"
               "                  (cl-loop for p across parts do\n"
               "                           (when (plist-get p :thoughtSignature) (cl-incf sigs))\n"
               "                           (when (plist-get p :functionCall) (cl-incf replayed-tool-calls))\n"
               "                           (when (eq (plist-get p :thought) t) (cl-incf thoughts))\n"
               "                           (when (and (stringp (plist-get p :text))\n"
               "                                      (or (string-prefix-p \"**Inspecting\" (plist-get p :text))\n"
               "                                          (string-prefix-p \"**Finalising\" (plist-get p :text)))\n"
               "                                      (not (eq (plist-get p :thought) t)))\n"
               "                             (cl-incf leaked-thoughts)))))\n"
               "              (prin1 (list :has-shadow-wire (and (fboundp 'macher-agent-wire-horizon-bounds)\n"
               "                                                 (bound-and-true-p macher-agent-wire-buffer-mode) t)\n"
               "                           :turn1-contents-bytes (length turn1-wire-json)\n"
               "                           :turn2-contents-bytes (length turn2-wire-json)\n"
               "                           :common-prefix-bytes cp\n"
               "                           :prefix-retention-pct (* 100.0 (/ (float cp) (max 1 (1- (length turn1-wire-json)))))\n"
               "                           :turn1-entries (length t1-vec)\n"
               "                           :turn2-entries (length t2-vec)\n"
               "                           :replayed-tool-calls replayed-tool-calls\n"
               "                           :preserved-signatures sigs\n"
               "                           :preserved-thoughts thoughts\n"
               "                           :downgraded-thoughts leaked-thoughts\n"
               "                           :user-buf-chars (buffer-size buf)\n"
               "                           :shadow-buf-chars (if (fboundp 'macher-agent-wire-total-chars)\n"
               "                                                 (macher-agent-wire-total-chars buf)\n"
               "                                               0))))\n"
               "            (terpri))))\n"
               "    (when (buffer-live-p buf) (kill-buffer buf))\n"
               "    (ignore-errors (delete-directory ws-dir t))))\n"))

            (cl-labels ((run-bench (load-root &optional verbose-ui disable-shadow extra-wire-dir)
                          (unless (file-exists-p (expand-file-name "macher-agent.el" load-root))
                            (when (not (file-exists-p (expand-file-name "macher-agent.elc" load-root)))
                              (error "load-root missing macher-agent.el: %S (files=%S)"
                                     (abbreviate-file-name load-root)
                                     (ignore-errors (directory-files load-root)))))
                          (with-temp-buffer
                            (let* ((env process-environment)
                                   (_ (when verbose-ui
                                        (setq env (cons "WIRE_BENCH_VERBOSE_UI=1" env))))
                                   (_ (when disable-shadow
                                        (setq env (cons "WIRE_BENCH_DISABLE_SHADOW=1" env))))
                                   (process-environment env)
                                   (args (append
                                          (list "-Q" "--batch"
                                                "-L" (directory-file-name load-root))
                                          (when (and extra-wire-dir (file-directory-p extra-wire-dir))
                                            (list "-L" (directory-file-name extra-wire-dir)))
                                          (list "-L" (directory-file-name gptel-dir)
                                                "--eval" "(setq load-prefer-newer t)"
                                                "-l" sim-script)))
                                   (code (apply #'call-process
                                                emacs-bin nil (current-buffer) nil args)))
                              (unless (= code 0)
                                (error "Bench subprocess failed (%s): %s" code (buffer-string)))
                              (goto-char (point-max))
                              (backward-sexp 1)
                              (read (current-buffer))))))
              (let* ((legacy-installed
                      (and installed-dir
                           (let ((core-el (expand-file-name "macher-agent-core.el" installed-dir)))
                             (and (file-exists-p core-el)
                                  (with-temp-buffer
                                    (insert-file-contents core-el)
                                    (not (search-forward "macher-agent-wire-buffer-mode" nil t)))))))
                     (old-default (if legacy-installed
                                      (run-bench installed-dir nil t nil)
                                    (run-bench new-dir nil t nil)))
                     (old-verbose (if legacy-installed
                                      (run-bench installed-dir t t nil)
                                    (run-bench new-dir t t nil)))
                     (new-default (run-bench new-dir nil nil wire-dir))
                     (new-verbose (run-bench new-dir t nil wire-dir)))
                ;; Old / legacy assertions (both default and input_diff.txt verbose modes)
                (expect (plist-get old-default :has-shadow-wire) :to-be nil)
                (expect (plist-get old-default :replayed-tool-calls) :to-equal 0)
                (expect (plist-get old-default :preserved-signatures) :to-equal 0)
                (expect (plist-get old-default :preserved-thoughts) :to-equal 0)
                (expect (< (plist-get old-default :common-prefix-bytes) 100) :to-be t)

                ;; Under input_diff.txt settings (reasoning=t, tool-results=t), all 10 thoughts downgrade
                ;; into visible model speech, 0 signatures survive, and user buffer bloats >25k chars!
                (expect (plist-get old-verbose :downgraded-thoughts) :to-equal 10)
                (expect (plist-get old-verbose :preserved-signatures) :to-equal 0)
                (expect (plist-get old-verbose :preserved-thoughts) :to-equal 0)
                (expect (plist-get old-verbose :replayed-tool-calls) :to-equal 9)
                (expect (< (plist-get old-verbose :common-prefix-bytes) 100) :to-be t)
                (expect (> (plist-get old-verbose :user-buf-chars) 25000) :to-be t)

                ;; New Shadow Wire assertions (100% lossless prefix + tiny user buffer in default clean mode!)
                (dolist (nm (list new-default new-verbose))
                  (expect (plist-get nm :has-shadow-wire) :to-be t)
                  (expect (plist-get nm :replayed-tool-calls) :to-equal 9)
                  (expect (plist-get nm :preserved-signatures) :to-equal 10)
                  (expect (plist-get nm :preserved-thoughts) :to-equal 10)
                  (expect (plist-get nm :downgraded-thoughts) :to-equal 0)
                  (expect (plist-get nm :common-prefix-bytes)
                          :to-equal (1- (plist-get nm :turn1-contents-bytes)))
                  (expect (plist-get nm :turn2-entries) :to-equal 21))
                (expect (< (plist-get new-default :user-buf-chars) 300) :to-be t))))
        (ignore-errors (delete-file sim-script)))))

  ;; --------------------------------------------------------------------------
  ;; WIR-07
  ;; --------------------------------------------------------------------------
  (it "WIR-07: pins quantised compaction epochs, deduplicates wire tool results without mutating wbuf, and handles gap/backend/char edge cases"
    (macher-agent-test-with-git-workspace (_root ctx _owner-buf
                                                 '(("macher-agent-core.el" . ";; SENTINEL_DUP_TOOL_7788\n(defun core () t)\n")))
      (let* ((gptel--known-backends nil)
             (orig-presets (copy-alist (default-value 'gptel--known-presets)))
             (orig-tools (copy-tree (default-value 'gptel--known-tools)))
             (gemini (gptel-make-gemini "WireGem07" :models '(gemini-3.1-pro-preview gemini-large) :stream t))
             (openai (gptel-make-openai "WireOAI07" :host "localhost" :models '(gpt-4o) :stream nil))
             (chat-buf (generate-new-buffer "*wir07-kv*")))
        (unwind-protect
            (progn
              (with-current-buffer chat-buf
                (setq-local default-directory (macher-agent-context-project-root ctx))
                (markdown-mode)
                (gptel-mode 1)
                (macher-agent-mode 1)
                (setq-local gptel-backend gemini)
                (setq-local gptel-model 'gemini-3.1-pro-preview)
                (setq-local gptel-include-reasoning nil)
                (setq-local gptel-include-tool-results nil)
                (macher-agent-initialize-skills ctx)
                (macher-agent-test--install-wire-ro-preset)

                ;; 1. Quantised compaction ratio pins horizon and alert across consecutive turns
                (let ((macher-agent-context-compaction-ratio 0.25)
                      (macher-agent-max-duplicate-tools 0)
                      (macher-agent-max-context-chars '((gemini-3.1-pro-preview . 800)
                                                        (gemini-large . 500000)
                                                        (nil . 2000000))))
                  (dotimes (i 3)
                    (goto-char (point-max))
                    (insert (format "### prompt %d with topic alpha_%d\n\n" (1+ i) (1+ i)))
                    (macher-agent-test--simulate-gemini-tool-turn
                     chat-buf gemini :steps 0 :stream t
                     :final-text (format "Reply %d covering details_alpha_%d_padding_for_budget." (1+ i) (1+ i))))
                  (goto-char (point-max))
                  (insert "turn 6 query\n\n")
                  (goto-char (point-max))
                  (let* ((fsm6 (gptel-request nil :dry-run t :stream t
                                              :transforms gptel-prompt-transform-functions))
                         (c6 (plist-get (plist-get (gptel-fsm-info fsm6) :data) :contents))
                         (alert6 (plist-get (aref (plist-get (aref c6 0) :parts) 0) :text))
                         (h-tid6 macher-agent-wire-horizon-turn-id)
                         (h-lines6 macher-agent-wire-horizon-lines-deleted)
                         (prefix6 (cl-subseq c6 0 (1- (length c6)))))
                    (expect h-tid6 :not :to-be nil)
                    (expect h-lines6 :to-be-greater-than 0)
                    (expect alert6 :to-match "SYSTEM ALERT: macher-agent truncated")
                    ;; Commit turn 6 and inspect turn 7 while remaining below max-chars:
                    ;; pinned horizon, line count, and prefix must stay byte-identical!
                    (macher-agent-test--simulate-gemini-tool-turn
                     chat-buf gemini :steps 0 :stream t :final-text "Reply 6 short.")
                    (goto-char (point-max))
                    (insert "turn 7 query\n\n")
                    (goto-char (point-max))
                    (let* ((fsm7 (gptel-request nil :dry-run t :stream t
                                                :transforms gptel-prompt-transform-functions))
                           (c7 (plist-get (plist-get (gptel-fsm-info fsm7) :data) :contents))
                           (prefix7 (cl-subseq c7 0 (length prefix6))))
                      (expect macher-agent-wire-horizon-turn-id :to-equal h-tid6)
                      (expect macher-agent-wire-horizon-lines-deleted :to-equal h-lines6)
                      (expect prefix7 :to-equal prefix6))

                    ;; Switching to larger model budget clears stale horizon in Step 55 before Step 75
                    (setq-local gptel-model 'gemini-large)
                    (goto-char (point-max))
                    (let* ((fsm-lg (gptel-request nil :dry-run t :stream t
                                                  :transforms gptel-prompt-transform-functions))
                           (c-lg (plist-get (plist-get (gptel-fsm-info fsm-lg) :data) :contents)))
                      (expect macher-agent-wire-horizon-turn-id :to-be nil)
                      (expect (plist-get (aref (plist-get (aref c-lg 0) :parts) 0) :text)
                              :not :to-match "SYSTEM ALERT"))))

                ;; 2. Epoch-pinned wire deduplication on Gemini thinking turns + Zero-Mem recall
                (erase-buffer)
                (macher-agent-wire-clear-buffer chat-buf)
                (setq-local gptel-model 'gemini-3.1-pro-preview)
                (let* ((wbuf (macher-agent-wire-buffer-for chat-buf t))
                       (macher-agent-max-duplicate-tools 1)
                       (macher-agent-context-compaction-ratio 0.25)
                       (big-out (concat "SENTINEL_DUP_TOOL_7788 " (make-string 220 ?x)))
                       (mk-gem-tool-turn
                        (lambda (tid-num args-plist)
                          (let ((ub (point-max)))
                            (insert (format "### check %d\n\n" tid-num))
                            (let ((ue (point-max))
                                  (rb (point-max)))
                              (insert (format "Done %d.\n\n" tid-num))
                              (let ((re (point-max))
                                    (u-ent (list (list :role "user"
                                                       :parts (vector (list :text (format "check %d" tid-num))))))
                                    (t-ent (list (list :role "model"
                                                       :parts (vector (list :text "thinking" :thought t)
                                                                      (list :functionCall
                                                                            (list :name "read_file_in_workspace"
                                                                                  :args args-plist)
                                                                            :thoughtSignature "SIG")))
                                                 (list :role "user"
                                                       :parts (vector (list :functionResponse
                                                                            (list :name "read_file_in_workspace"
                                                                                  :response (list :name "read_file_in_workspace"
                                                                                                  :content big-out)))))))
                                    (f-ent (list (list :role "model"
                                                       :parts (vector (list :text (format "Done %d." tid-num)))))))
                                (let ((ubm (copy-marker ub nil)) (uem (copy-marker ue nil))
                                      (rbm (copy-marker rb nil)) (rem (copy-marker re nil)))
                                  (with-current-buffer wbuf
                                    (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                                      (goto-char (point-max))
                                      (let ((p0 (point)))
                                        (insert (format "### check %d\n\n" tid-num))
                                        (macher-agent-wire-annotate-shadow-span
                                         wbuf p0 (point)
                                         (list 'macher-agent-turn-id tid-num
                                               'macher-agent-span-kind 'user-prompt
                                               'macher-agent-wire-entries u-ent
                                               'macher-agent-wire-backend-type 'gptel-gemini
                                               'macher-agent-wire-chars (macher-agent-wire-estimate-chars u-ent)
                                               'macher-agent-orig-beg ubm 'macher-agent-orig-end uem
                                               'macher-agent-orig-nonempty t
                                               'macher-agent-norm-hash (macher-agent-wire-norm-hash chat-buf ubm uem))))
                                      (let ((p1 (point)))
                                        (insert (format "``` tool (read_file_in_workspace)\n%s\n```\n\n" big-out))
                                        (macher-agent-wire-annotate-shadow-span
                                         wbuf p1 (point)
                                         (list 'macher-agent-turn-id tid-num
                                               'macher-agent-span-kind 'tool-step
                                               'macher-agent-wire-entries t-ent
                                               'macher-agent-wire-backend-type 'gptel-gemini
                                               'macher-agent-wire-chars (macher-agent-wire-estimate-chars t-ent)
                                               'macher-agent-orig-beg rbm 'macher-agent-orig-end rbm
                                               'macher-agent-orig-nonempty nil
                                               'macher-agent-norm-hash (sxhash-equal ""))))
                                      (let ((p2 (point)))
                                        (insert (format "Done %d.\n\n" tid-num))
                                        (put-text-property p2 (point) 'gptel 'response)
                                        (macher-agent-wire-annotate-shadow-span
                                         wbuf p2 (point)
                                         (list 'macher-agent-turn-id tid-num
                                               'macher-agent-span-kind 'model-final
                                               'macher-agent-wire-entries f-ent
                                               'macher-agent-wire-backend-type 'gptel-gemini
                                               'macher-agent-wire-chars (macher-agent-wire-estimate-chars f-ent)
                                               'macher-agent-orig-beg rbm 'macher-agent-orig-end rem
                                               'macher-agent-orig-nonempty t
                                               'macher-agent-norm-hash (macher-agent-wire-norm-hash chat-buf rbm rem)))))))))))))
                  ;; Two identical tool calls with reordered :args keys -> deduplication keeps Turn 2, omits Turn 1
                  (funcall mk-gem-tool-turn 1 '(:path "a.el" :limit 10))
                  (funcall mk-gem-tool-turn 2 '(:limit 10 :path "a.el"))
                  (setq-local macher-agent-wire-turn-counter 2)
                  (setq-local macher-agent-wire-backend gemini)
                  (let* ((all-chars (macher-agent-wire-total-chars chat-buf))
                         ;; Set max-chars just below total-all so dedup drops rem <= max-chars without truncating Turn 1!
                         (macher-agent-max-context-chars (list (cons 'gemini-3.1-pro-preview (- all-chars 50)))))
                    (goto-char (point-max))
                    (insert "follow up 3\n\n")
                    (goto-char (point-max))
                    (let* ((fsm-d1 (gptel-request nil :dry-run t :stream t
                                                  :transforms gptel-prompt-transform-functions))
                           (c-d1 (plist-get (plist-get (gptel-fsm-info fsm-d1) :data) :contents))
                           (t1-resp (plist-get (plist-get (aref (plist-get (aref c-d1 2) :parts) 0) :functionResponse) :response))
                           (t2-resp (plist-get (plist-get (aref (plist-get (aref c-d1 6) :parts) 0) :functionResponse) :response)))
                      (expect macher-agent-wire-dedup-turn-id :to-equal 2)
                      (expect macher-agent-wire-horizon-turn-id :to-be nil)
                      (expect (plist-get t1-resp :content) :to-match "duplicate")
                      (expect (plist-get t2-resp :content) :to-match "SENTINEL_DUP_TOOL_7788")
                      ;; Raw wbuf text remains immutable and glob search recalls the omitted tool output despite no truncation!
                      (expect (with-current-buffer wbuf (buffer-string)) :to-match "SENTINEL_DUP_TOOL_7788")
                      (expect (macher-agent-search-glob chat-buf "SENTINEL_DUP_TOOL_7788")
                              :to-match "SENTINEL_DUP_TOOL_7788")
                      ;; Next turn keeps the pinned dedup prefix byte-identical even though rem <= max-chars
                      (let ((fsm-d2 (gptel-request nil :dry-run t :stream t
                                                   :transforms gptel-prompt-transform-functions)))
                        (expect (plist-get (plist-get (gptel-fsm-info fsm-d2) :data) :contents)
                                :to-equal c-d1)))))

                ;; 3. Anthropic, OpenAI Chat, and OpenAI Responses ID correlation in deduplication
                (let ((macher-agent-max-duplicate-tools 1))
                  (let* ((b-anth (list (list :turn-id 1
                                             :entries (list (list :role "assistant"
                                                                  :content (vector (list :type "tool_use" :id "tu_1" :name "t" :input '(:a 1))))
                                                            (list :role "user"
                                                                  :content (vector (list :type "tool_result" :tool_use_id "tu_1" :content "OLD_ANTH"))))
                                             :chars 100)
                                       (list :turn-id 2
                                             :entries (list (list :role "assistant"
                                                                  :content (vector (list :type "tool_use" :id "tu_2" :name "t" :input '(:a 1))))
                                                            (list :role "user"
                                                                  :content (vector (list :type "tool_result" :tool_use_id "tu_2" :content "NEW_ANTH"))))
                                             :chars 100)))
                         (d-anth (macher-agent-wire-deduplicate-bundles b-anth 2)))
                    (expect (plist-get (aref (plist-get (nth 1 (plist-get (nth 0 d-anth) :entries)) :content) 0) :content)
                            :to-match "duplicate")
                    (expect (plist-get (aref (plist-get (nth 1 (plist-get (nth 1 d-anth) :entries)) :content) 0) :content)
                            :to-equal "NEW_ANTH")
                    ;; Original b-anth remained unmutated
                    (expect (plist-get (aref (plist-get (nth 1 (plist-get (nth 0 b-anth) :entries)) :content) 0) :content)
                            :to-equal "OLD_ANTH"))
                  (let* ((b-oai (list (list :turn-id 1
                                            :entries (list (list :role "assistant"
                                                                 :tool_calls (vector (list :id "c1" :function (list :name "t" :arguments "{\"b\":2,\"a\":1}"))))
                                                           (list :role "tool" :tool_call_id "c1" :content "OLD_OAI"))
                                            :chars 100)
                                      (list :turn-id 2
                                            :entries (list (list :role "assistant"
                                                                 :tool_calls (vector (list :id "c2" :function (list :name "t" :arguments "{\"a\":1,\"b\":2}"))))
                                                           (list :role "tool" :tool_call_id "c2" :content "NEW_OAI"))
                                            :chars 100)))
                         (d-oai (macher-agent-wire-deduplicate-bundles b-oai 2)))
                    (expect (plist-get (nth 1 (plist-get (nth 0 d-oai) :entries)) :content) :to-match "duplicate")
                    (expect (plist-get (nth 1 (plist-get (nth 1 d-oai) :entries)) :content) :to-equal "NEW_OAI"))
                  (let* ((b-resp (list (list :turn-id 1
                                             :entries (list (list :type "function_call" :call_id "r1" :name "t" :arguments "{\"a\":1}")
                                                            (list :type "function_call_output" :call_id "r1" :output "OLD_RESP"))
                                             :chars 100)
                                       (list :turn-id 2
                                             :entries (list (list :type "function_call" :call_id "r2" :name "t" :arguments "{\"a\":1}")
                                                            (list :type "function_call_output" :call_id "r2" :output "NEW_RESP"))
                                             :chars 100)))
                         (d-resp (macher-agent-wire-deduplicate-bundles b-resp 2)))
                    (expect (plist-get (nth 1 (plist-get (nth 0 d-resp) :entries)) :output) :to-match "duplicate")
                    (expect (plist-get (nth 1 (plist-get (nth 1 d-resp) :entries)) :output) :to-equal "NEW_RESP")))

                ;; 4. Equal-size adjacent spans summed accurately, silent hook invalidation, tagged gap horizon, and pre-pipeline backend switch
                (erase-buffer)
                (macher-agent-wire-clear-buffer chat-buf)
                (let ((wbuf (macher-agent-wire-buffer-for chat-buf t))
                      (msg-pos-before (with-current-buffer "*Messages*" (point-max))))
                  (with-current-buffer wbuf
                    (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                      (let ((p1 (point)))
                        (insert "gap1\n\n")
                        (put-text-property p1 (point) 'macher-agent-span-id 1)
                        (put-text-property p1 (point) 'macher-agent-turn-id '(gap . 1))
                        (put-text-property p1 (point) 'macher-agent-wire-chars 123))
                      (let ((p2 (point)))
                        (insert "gap2\n\n")
                        (put-text-property p2 (point) 'macher-agent-span-id 2)
                        (put-text-property p2 (point) 'macher-agent-turn-id '(gap . 2))
                        (put-text-property p2 (point) 'macher-agent-wire-chars 123))))
                  ;; Adjacent equal-size spans must sum to 246, not coalesce to 123
                  (expect (macher-agent-wire-total-chars chat-buf) :to-equal 246)
                  ;; Tagged gap horizon stores (gap . span-id), never an integer offset
                  (let* ((gbundles (list (list :turn-id '(gap . 1) :entries '((:role "user" :content "g1")) :chars 123 :sbeg 1 :send 7)
                                         (list :turn-id '(gap . 2) :entries '((:role "user" :content "g2")) :chars 123 :sbeg 7 :send 13)
                                         (list :turn-id 1 :entries '((:role "user" :content "t1")) :chars 50 :sbeg 13 :send 20)))
                         (macher-agent-context-compaction-ratio 0.25)
                         (res-g (macher-agent-wire-resolve-horizon chat-buf gbundles 160)))
                    (expect macher-agent-wire-horizon-turn-id :to-equal '(gap . 2))
                    (expect (length (plist-get res-g :retained)) :to-equal 1))
                  ;; Clearing wire buffer fires macher-agent-wire-invalidated-hook cleanly without noise
                  (let* ((inv-args nil)
                         (macher-agent-wire-invalidated-hook
                          (list (lambda (buf reason) (setq inv-args (list buf reason))))))
                    (expect (plist-get (macher-agent-wire-horizon-bounds chat-buf 160) :wire-managed) :to-be t)
                    (macher-agent-wire-clear-buffer chat-buf)
                    (expect (macher-agent-wire-horizon-bounds chat-buf 160) :to-be nil)
                    (expect inv-args :to-equal (list chat-buf 'cleared))
                    (expect (with-current-buffer "*Messages*"
                              (buffer-substring-no-properties
                               (min (point-max) (max (point-min) msg-pos-before))
                               (point-max)))
                            :not :to-match "Zero-Mem graph and vector storage cleared"))
                  ;; Pre-pipeline local backend switch clears wbuf before Step 55
                  (setq-local macher-agent-wire-backend gemini)
                  (setq-local gptel-backend openai)
                  (setq-local gptel-model 'gpt-4o)
                  (insert "switch to openai\n\n")
                  (goto-char (point-max))
                  (gptel-request nil :dry-run t :stream nil
                                 :transforms gptel-prompt-transform-functions)
                  (expect (macher-agent-wire-has-spans-p chat-buf) :to-be nil))

                ;; 5. Intra-turn gap isolation, mid-span cursor protection, and untracked->skill backend switch
                (erase-buffer)
                (macher-agent-wire-clear-buffer chat-buf)
                (setq-local gptel-backend gemini)
                (setq-local gptel-model 'gemini-3.1-pro-preview)
                (let ((wbuf (macher-agent-wire-buffer-for chat-buf t))
                      (gptel--known-backends `(("WireGem07" . ,gemini) ("WireOAI07" . ,openai))))
                  ;; 5a. Intra-turn gap spans do not duplicate u-sp or prune enclosing turn on deletion
                  (insert "### user prompt ALPHA_U1\n")
                  (let ((u-beg (copy-marker 1 nil))
                        (u-end (copy-marker (point) nil))
                        (g1-beg (copy-marker (point) nil)))
                    (insert "intra gap one INTRA_G1\n")
                    (let ((g1-end (copy-marker (point) nil))
                          (f-beg (copy-marker (point) nil)))
                      (insert "final reply F1\n")
                      (put-text-property f-beg (point) 'gptel 'response)
                      (let ((f-end (copy-marker (point) nil)))
                        (with-current-buffer wbuf
                          (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                            (goto-char (point-max))
                            (let ((p0 (point)))
                              (insert "### user prompt ALPHA_U1\n\n")
                              (macher-agent-wire-annotate-shadow-span
                               wbuf p0 (point)
                               (list 'macher-agent-turn-id 1 'macher-agent-span-kind 'user-prompt
                                     'macher-agent-wire-entries '((:role "user" :parts [(:text "user prompt ALPHA_U1")]))
                                     'macher-agent-wire-backend-type 'gptel-gemini 'macher-agent-wire-chars 40
                                     'macher-agent-orig-beg u-beg 'macher-agent-orig-end u-end
                                     'macher-agent-orig-nonempty t
                                     'macher-agent-norm-hash (macher-agent-wire-norm-hash chat-buf u-beg u-end))))
                            (let ((p1 (point)))
                              (insert "intra gap one INTRA_G1\n\n")
                              (macher-agent-wire-annotate-shadow-span
                               wbuf p1 (point)
                               (list 'macher-agent-turn-id 1 'macher-agent-span-kind 'gap
                                     'macher-agent-wire-entries '((:role "user" :parts [(:text "intra gap one INTRA_G1")]))
                                     'macher-agent-wire-backend-type 'gptel-gemini 'macher-agent-wire-chars 40
                                     'macher-agent-orig-beg g1-beg 'macher-agent-orig-end g1-end
                                     'macher-agent-orig-nonempty t
                                     'macher-agent-norm-hash (macher-agent-wire-norm-hash chat-buf g1-beg g1-end))))
                            (let ((p2 (point)))
                              (insert "final reply F1\n\n")
                              (put-text-property p2 (point) 'gptel 'response)
                              (macher-agent-wire-annotate-shadow-span
                               wbuf p2 (point)
                               (list 'macher-agent-turn-id 1 'macher-agent-span-kind 'model-final
                                     'macher-agent-wire-entries '((:role "model" :parts [(:text "final reply F1")]))
                                     'macher-agent-wire-backend-type 'gptel-gemini 'macher-agent-wire-chars 30
                                     'macher-agent-orig-beg f-beg 'macher-agent-orig-end f-end
                                     'macher-agent-orig-nonempty t
                                     'macher-agent-norm-hash (macher-agent-wire-norm-hash chat-buf f-beg f-end))))))
                        ;; Syncing twice must NOT duplicate u-sp or insert extra spans
                        (macher-agent-wire-sync-spans chat-buf (point-max) gemini)
                        (macher-agent-wire-sync-spans chat-buf (point-max) gemini)
                        (expect (length (macher-agent-wire-collect-spans wbuf)) :to-equal 3)
                        ;; Mid-span cursor send inside u-sp must NOT overwrite u-sp with truncated prefix
                        (let ((orig-u-hash (plist-get (car (macher-agent-wire-collect-spans wbuf)) :norm-hash)))
                          (macher-agent-wire-sync-spans chat-buf (+ (marker-position u-beg) 8) gemini)
                          (expect (plist-get (car (macher-agent-wire-collect-spans wbuf)) :norm-hash)
                                  :to-equal orig-u-hash))
                        ;; Deleting intra-turn gap INTRA_G1 prunes ONLY the gap span while keeping u-sp and f-sp
                        (delete-region (marker-position g1-beg) (marker-position g1-end))
                        (macher-agent-wire-sync-spans chat-buf (point-max) gemini)
                        (let ((rem-spans (macher-agent-wire-collect-spans wbuf)))
                          (expect (length rem-spans) :to-equal 2)
                          (expect (mapcar (lambda (s) (plist-get s :kind)) rem-spans)
                                  :to-equal '(user-prompt model-final))))))
                  ;; 5b. Untracked pre-history + skill backend switch rebuilds gap span under new skill backend
                  (erase-buffer)
                  (macher-agent-wire-clear-buffer chat-buf)
                  (setf (alist-get 'wir-oai-skill gptel--known-presets)
                        '(:system "OAI skill" :backend "WireOAI07" :model gpt-4o))
                  (insert "Prior untracked history.\n")
                  (let ((sp (point)))
                    (insert "\n")
                    (add-text-properties sp (point) '(gptel ignore rear-nonsticky (gptel))))
                  (insert "### @wir-oai-skill switch now\n\n")
                  (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore)
                            ((symbol-function 'gptel--url-get-response) #'ignore))
                    (goto-char (point-max))
                    (gptel-request nil :stream nil :transforms gptel-prompt-transform-functions)
                    (expect macher-agent-wire-backend :to-be openai)
                    (let ((g-sp (car (macher-agent-wire-collect-spans wbuf))))
                      (expect (plist-get g-sp :kind) :to-equal 'gap)
                      (expect (plist-get g-sp :backend-type) :to-equal 'gptel-openai))))))
          (when (buffer-live-p chat-buf) (kill-buffer chat-buf))
          (set-default 'gptel--known-presets orig-presets)
          (set-default 'gptel--known-tools orig-tools)))))

  ;; --------------------------------------------------------------------------
  ;; WIR-08: macher-agent-wire-rebuild-prompts preserves horizon max-chars & disable-local-tools
  ;; --------------------------------------------------------------------------
  (it "WIR-08: rebuild-prompts preserves horizon truncation (max-chars) and disable-local-tools"
    (let* ((gemini (gptel-make-gemini "WireRebuildGem08"
                                      :models '(gemini-3.1-pro-preview)
                                      :stream t))
           (chat-buf (generate-new-buffer "*wir-08-rebuild*")))
      (unwind-protect
          (with-current-buffer chat-buf
            (text-mode)
            (gptel-mode 1)
            (setq-local gptel-backend gemini)
            (setq-local gptel-model 'gemini-3.1-pro-preview)
            (setq-local macher-agent-wire-backend gemini)
            (let ((wbuf (macher-agent-wire-buffer-for chat-buf t))
                  (macher-agent-max-context-chars '((gemini-3.1-pro-preview . 180) (nil . 2000000)))
                  (macher-agent-context-compaction-ratio 0.25))
              (cl-flet ((add-span (text turn-id kind entries chars response-p &optional shadow-text)
                          (let ((obeg (copy-marker (point) nil))
                                (nonempty (not (string-empty-p text))))
                            (when nonempty
                              (insert text)
                              (when response-p
                                (put-text-property obeg (point) 'gptel 'response)))
                            (let ((oend (copy-marker (point) nil))
                                  (nhash (if nonempty
                                             (macher-agent-wire-norm-hash chat-buf obeg (point))
                                           (sxhash-equal "")))
                                  (wtext (or shadow-text text)))
                              (with-current-buffer wbuf
                                (let ((inhibit-read-only t)
                                      (inhibit-modification-hooks t))
                                  (goto-char (point-max))
                                  (let ((wbeg (point)))
                                    (insert wtext)
                                    (when response-p
                                      (put-text-property wbeg (point) 'gptel 'response))
                                    (macher-agent-wire-annotate-shadow-span
                                     wbuf wbeg (point)
                                     (list 'macher-agent-turn-id turn-id
                                           'macher-agent-span-kind kind
                                           'macher-agent-wire-entries entries
                                           'macher-agent-wire-backend-type 'gptel-gemini
                                           'macher-agent-wire-chars chars
                                           'macher-agent-orig-beg obeg
                                           'macher-agent-orig-end oend
                                           'macher-agent-orig-nonempty nonempty
                                           'macher-agent-norm-hash nhash)))))))))
                (add-span "### OLD_TURN_1_ALPHA\n\n" 1 'user-prompt
                          '((:role "user" :parts [(:text "OLD_TURN_1_ALPHA")])) 100 nil)
                (add-span "OLD_REPLY_1\n\n" 1 'model-final
                          '((:role "model" :parts [(:text "OLD_REPLY_1")])) 100 t)
                (add-span "### OLD_TURN_2_BETA\n\n" 2 'user-prompt
                          '((:role "user" :parts [(:text "OLD_TURN_2_BETA")])) 100 nil)
                (add-span "OLD_REPLY_2\n\n" 2 'model-final
                          '((:role "model" :parts [(:text "OLD_REPLY_2")])) 100 t)
                (add-span "### RECENT_TURN_3_GAMMA\n\n" 3 'user-prompt
                          '((:role "user" :parts [(:text "RECENT_TURN_3_GAMMA")])) 40 nil)
                (add-span "" 3 'tool-step
                          '((:role "model"
                                   :parts [(:functionCall (:name "wire_read_tool" :args (:path "foo.el"))
                                                          :id "call_wir08_3")])
                            (:role "user"
                                   :parts [(:functionResponse (:name "wire_read_tool"
                                                               :id "call_wir08_3"
                                                               :response (:content "TOOL_BODY_3")))]))
                          40 nil
                          "``` tool (wire_read_tool)\nTOOL_BODY_3\n```\n\n")
                (add-span "RECENT_REPLY_3\n\n" 3 'model-final
                          '((:role "model" :parts [(:text "RECENT_REPLY_3")])) 40 t))
              (let ((cur-beg (point)))
                (insert "### CURRENT_PROMPT_DELTA\n")
                ;; 1. With local tools enabled: horizon truncates old turns while retaining tool calls
                (let* ((rebuilt (macher-agent-wire-rebuild-prompts
                                 chat-buf gemini nil
                                 (list :model 'gemini-3.1-pro-preview
                                       :disable-local-tools nil
                                       :macher-agent-prompt-bounds (cons cur-beg (point-max)))))
                       (printed (prin1-to-string rebuilt)))
                  (expect macher-agent-wire-horizon-turn-id :to-equal 2)
                  (expect printed :not :to-match "OLD_TURN_1_ALPHA")
                  (expect printed :not :to-match "OLD_TURN_2_BETA")
                  (expect printed :to-match "RECENT_TURN_3_GAMMA")
                  (expect printed :to-match "CURRENT_PROMPT_DELTA")
                  (expect printed :to-match ":functionCall")
                  (expect printed :to-match ":functionResponse"))
                ;; 2. With :disable-local-tools t: preserves horizon AND flattens Gemini tool parts
                (let* ((rebuilt-toolless (macher-agent-wire-rebuild-prompts
                                          chat-buf gemini nil
                                          (list :model 'gemini-3.1-pro-preview
                                                :disable-local-tools t
                                                :macher-agent-prompt-bounds (cons cur-beg (point-max)))))
                       (printed-toolless (prin1-to-string rebuilt-toolless)))
                  (expect printed-toolless :not :to-match "OLD_TURN_1_ALPHA")
                  (expect printed-toolless :not :to-match "OLD_TURN_2_BETA")
                  (expect printed-toolless :to-match "RECENT_TURN_3_GAMMA")
                  (expect printed-toolless :to-match "CURRENT_PROMPT_DELTA")
                  (expect printed-toolless :not :to-match ":functionCall")
                  (expect printed-toolless :not :to-match ":functionResponse")
                  (expect printed-toolless :to-match "\\[Prior tool call wire_read_tool")
                  (expect printed-toolless :to-match "\\[Prior tool result wire_read_tool")))))
        (when (buffer-live-p chat-buf) (kill-buffer chat-buf)))))

  ;; --------------------------------------------------------------------------
  ;; WIR-09: macher-agent-wire-uninstall preserves custom hooks/protocol and re-install is idempotent
  ;; --------------------------------------------------------------------------
  (it "WIR-09: uninstall preserves custom hooks/protocol overrides and install is idempotent"
    (let ((custom-hijack (lambda (_fsm _buf) nil))
          (custom-transform (lambda (_fn _fsm) nil))
          (custom-hist-fn (lambda (_buf) 12345)))
      (unwind-protect
          (progn
            ;; Calling install repeatedly must be idempotent without duplicate hooks
            (macher-agent-wire-install)
            (macher-agent-wire-install)
            (expect (cl-count #'macher-agent-wire-finalize-transform gptel-prompt-transform-functions)
                    :to-equal 1)
            (expect (cl-count #'macher-agent-wire--on-fsm-hijack macher-agent-fsm-hijack-hook)
                    :to-equal 1)
            (expect (cl-count #'macher-agent-wire--on-pre-prompt-compile macher-agent-pre-prompt-compile-hook)
                    :to-equal 1)
            (expect (cl-count #'macher-agent-wire--on-prepare-prompt macher-agent-prepare-wire-prompt-hook)
                    :to-equal 1)
            (expect (cl-count #'macher-agent-wire--apply-prefix-to-fsm macher-agent-post-prompt-transform-hook)
                    :to-equal 1)
            (expect (cl-count #'macher-agent-wire-clone-buffer macher-agent-branch-chat-hook)
                    :to-equal 1)
            (expect (cl-count #'macher-agent-wire-clear-buffer macher-agent-backend-changed-hook)
                    :to-equal 1)

            ;; Register custom hooks and override one protocol variable before uninstall
            (add-hook 'macher-agent-fsm-hijack-hook custom-hijack)
            (add-hook 'gptel-prompt-transform-functions custom-transform)
            (setq macher-agent-history-chars-function custom-hist-fn)

            (macher-agent-wire-uninstall)

            ;; All wire hooks removed; custom hooks preserved
            (expect (memq #'macher-agent-wire-finalize-transform gptel-prompt-transform-functions) :to-be nil)
            (expect (memq #'macher-agent-wire--on-fsm-hijack macher-agent-fsm-hijack-hook) :to-be nil)
            (expect (memq #'macher-agent-wire--on-pre-prompt-compile macher-agent-pre-prompt-compile-hook) :to-be nil)
            (expect (memq #'macher-agent-wire--on-prepare-prompt macher-agent-prepare-wire-prompt-hook) :to-be nil)
            (expect (memq #'macher-agent-wire--apply-prefix-to-fsm macher-agent-post-prompt-transform-hook) :to-be nil)
            (expect (memq #'macher-agent-wire-clone-buffer macher-agent-branch-chat-hook) :to-be nil)
            (expect (memq #'macher-agent-wire-clear-buffer macher-agent-backend-changed-hook) :to-be nil)
            (expect (memq custom-hijack macher-agent-fsm-hijack-hook) :to-be-truthy)
            (expect (memq custom-transform gptel-prompt-transform-functions) :to-be-truthy)

            ;; Custom protocol override preserved; wire-owned protocol vars restored to defaults
            (expect macher-agent-history-chars-function :to-be custom-hist-fn)
            (expect macher-agent-conversation-source-buffer-function :to-be #'identity)
            (expect macher-agent-horizon-bounds-function :to-be nil)
            (expect macher-agent-has-prior-response-function :to-be nil)
            (expect macher-agent-last-committed-pos-function :to-be nil)
            (expect macher-agent-strip-wire-vars-function :to-be nil)
            (expect macher-agent-rebuild-prompts-function :to-be nil)

            ;; Re-install idempotently restores wire protocol and hooks while keeping custom hooks
            (macher-agent-wire-install)
            (macher-agent-wire-install)
            (expect macher-agent-history-chars-function :to-be #'macher-agent-wire-history-chars)
            (expect macher-agent-rebuild-prompts-function :to-be #'macher-agent-wire-rebuild-prompts)
            (expect (cl-count #'macher-agent-wire-finalize-transform gptel-prompt-transform-functions)
                    :to-equal 1)
            (expect (memq custom-hijack macher-agent-fsm-hijack-hook) :to-be-truthy)
            (expect (memq custom-transform gptel-prompt-transform-functions) :to-be-truthy)

            ;; Uninstalling when history-chars-function is wire-owned restores core default
            (macher-agent-wire-uninstall)
            (expect macher-agent-history-chars-function :to-be #'macher-agent-default-history-chars))
        (remove-hook 'macher-agent-fsm-hijack-hook custom-hijack)
        (remove-hook 'gptel-prompt-transform-functions custom-transform)
        (macher-agent-wire-install)))))

(provide 'test-wire)
;;; test-wire.el ends here
