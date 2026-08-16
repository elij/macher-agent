;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
    macher-agent-delegate-tasks-to-subagents-tool
    "Delegate tasks to multiple sub-agents asynchronously using A2A point-to-point payloads. \
Use this tool if you need feedback from the agent after execution."
  :category "collaboration"
  :args '((:name "tasks" :type array :items
                 (:type object :properties
                        (:buffer_name (:type string)
                                      :instructions (:type string)
                                      :presets (:type array :items (:type string))
                                      :ephemeral (:type boolean :description "Whether sub-agent should be reaped immediately upon task completion.")))))
  :command-fn
  (lambda (payload context _root callback)
    (let*
        ((raw-tasks (plist-get payload :tasks))
         (a2a-payloads
          (cl-loop
           for task-obj in (append raw-tasks nil)
           collect
           (let
               ((task-id
                 (if (fboundp 'org-id-uuid)
                     (org-id-uuid)
                   (format "task-%04x" (random #xffff)))))
             (list
              :type 'SEND_MESSAGE
              :task-id task-id
              :message (list :instructions (plist-get task-obj :instructions))
              :metadata (list :buffer_name (plist-get task-obj :buffer_name)
                              :presets (append (plist-get task-obj :presets) nil)
                              :suppress-patch t
                              :ephemeral (plist-get task-obj :ephemeral)))))))
      (macher-agent-a2a-dispatch 
       a2a-payloads 
       (lambda (a2a-results)
         (let
             ((lisp-results
               (cl-loop
                for res across (if (vectorp a2a-results) a2a-results (vconcat a2a-results))
                collect
                (let*
                    ((msg (plist-get res :message))
                     (data (or (plist-get res :data) (plist-get msg :data)))
                     (status (or (plist-get res :status) (plist-get msg :status)))
                     (buf-name
                      (or
                       (plist-get res :buffer-name) (plist-get msg :buffer-name) "sub-agent"))
                     (err (or (plist-get res :error) (plist-get msg :error))))
                  (if (eq status 'success)
                      (list :status 'success :data data :buffer-name buf-name)
                    (list :status 'error :error err :buffer-name buf-name))))))
           (funcall callback (vconcat lisp-results))))
       context)))
  :success-fn
  (lambda (results)
    (let ((output (list "All sub-agents completed:\n\n")))
      (cl-loop for res across (if (vectorp results) results (vconcat results))
               do (push
                   (format "=== Response from %s ===\n%s\n" 
                           (plist-get res :buffer-name)
                           (if (eq (plist-get res :status) 'success)
                               (plist-get res :data)
                             (plist-get res :error)))
                   output))
      (string-join (nreverse output) "\n"))))
