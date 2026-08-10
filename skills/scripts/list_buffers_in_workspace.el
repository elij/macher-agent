;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
 macher-agent-list-buffers-in-workspace-tool
 "List all buffers you currently have explicit access to. You cannot access buffers outside this list."
 :category "perception"
 :args nil
 :command-fn
 (lambda (_payload context root-dir)
   (when-let*
       ((entries
         (and context (macher-agent--get-context-contents context))))
     (let (acc)
       (dolist (entry entries (nreverse acc))
         (let* ((buf-name (macher-agent-vfs-entry-path entry))
                (classification
                 (macher-agent-context-classify-entry buf-name root-dir)))
           (when (memq classification '(buffer external))
             (push buf-name acc)))))))
 :success-fn
 (lambda (active-buffers _payload)
   (if active-buffers
       (mapconcat #'identity active-buffers "\n")
     "No buffers are currently in your scope.")))
