;;; list_available_tools.el --- List available tools -*- lexical-binding: t; -*-

(setq macher-agent-list-available-tools-tool
      (gptel-make-tool
       :name "list_available_tools"
       :description "List all registered gptel-tools in the current workspace. Use this to discover tools to assign to a preset."
       :category "meta"
       :args nil
       :async t
       :function (macher-agent-with-presentation-context ()
                   (let* ((native-fn (get 'macher-agent-list-available-tools-tool 'ptc-function))
                          (res (funcall native-fn context)))
                     (format "SUCCESS:\n%s" res)))))

(put 'macher-agent-list-available-tools-tool 'ptc-function
     (lambda (&optional _context)
       (let ((tools-list nil)
             (allowed-categories '("macher" "event" "perception" "collaboration" "execution")))
         (when (bound-and-true-p gptel--known-tools)
           (dolist (cat-entry gptel--known-tools)
             (let* ((cat-raw (car cat-entry))
                    (cat-name (if (symbolp cat-raw) (symbol-name cat-raw) cat-raw))
                    (tools-alist (cdr cat-entry)))
               (when (member cat-name allowed-categories)
                 (dolist (tool-entry tools-alist)
                   (let* ((name (car tool-entry))
                          (tool (cdr tool-entry))
                          (name-str (if (stringp name) name (symbol-name name)))
                          (desc (if (and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
                                    (gptel-tool-description tool)
                                  "")))
                     (push (format "- %s: %s" name-str desc) tools-list)))))))
         (if tools-list
             (string-join (nreverse tools-list) "\n")
           "No tools registered."))))
