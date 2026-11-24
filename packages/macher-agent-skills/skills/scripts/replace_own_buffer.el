;;; replace_own_buffer.el --- Replace current buffer content tool -*- lexical-binding: t; -*-

(setq macher-agent-replace-own-buffer-tool
      (gptel-make-tool
       :name "replace_own_buffer"
       :description "Replace the contents of the agent's current buffer."
       :category "execution"
       :include nil
       :args '((:name "content"
                      :type "string"
                      :description "The new content to insert into the buffer"))
       :async t
       :function (macher-agent-with-presentation-context (content)
                   (let ((target-buf (current-buffer))
                         (new-content (or content "")))
                     (letrec ((hook-fn
                               (lambda (_beg _end)
                                 (when (buffer-live-p target-buf)
                                   (with-current-buffer target-buf
                                     (remove-hook 'gptel-post-response-functions hook-fn t)
                                     (let ((inhibit-read-only t))
                                       (erase-buffer)
                                       (set (make-local-variable 'gptel--bounds) nil)
                                       (when (fboundp 'macher-agent-wire-clear-buffer)
                                         (funcall 'macher-agent-wire-clear-buffer target-buf))
                                       (insert new-content)
                                       (when (fboundp 'gptel-prompt-prefix-string)
                                         (insert "\n\n" (gptel-prompt-prefix-string)))
                                       (goto-char (point-max))))))))
                       (with-current-buffer target-buf
                         (add-hook 'gptel-post-response-functions hook-fn t t))
                       (format "SUCCESS: Buffer '%s' scheduled for replacement upon turn completion."
                               (buffer-name target-buf)))))))
