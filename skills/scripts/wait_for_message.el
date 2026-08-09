;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
    macher-agent-wait-for-message-tool
    "Suspends the agent until a message, task, or payload is received from a sender."
  :category "event"
  :args '()
  :command-fn
  (lambda (_payload _context _root)
    (let ((my-buf-name (buffer-name)))
      (lambda (resolve-fn)
        (puthash my-buf-name resolve-fn macher-agent--pending-callbacks))))
  :success-fn
  (lambda (result)
    (format "SYSTEM: Message received! Waking up.\n\n=== MESSAGE ===\n%s" result)))
