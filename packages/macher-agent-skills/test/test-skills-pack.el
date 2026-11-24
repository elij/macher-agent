;;; test-skills-pack.el --- Tests for macher-agent-skills package -*- lexical-binding: t; -*-

(unless (featurep 'macher-agent-skills-test-setup)
  (load (expand-file-name "macher-agent-skills-test-setup.el"
                          (file-name-directory (or load-file-name buffer-file-name)))
        nil t))

(describe "macher-agent-skills package"
  (it "SKLP-01: install and uninstall manage plugin skill directory idempotently"
    (let ((macher-agent-plugin-skill-directories nil)
          (macher-agent-install-hook nil))
      (macher-agent-skills-install)
      (macher-agent-skills-install)
      (expect (length macher-agent-plugin-skill-directories) :to-equal 1)
      (expect (member (file-name-as-directory (expand-file-name macher-agent-skills-directory))
                      macher-agent-plugin-skill-directories)
              :to-be-truthy)
      (macher-agent-skills-uninstall)
      (expect macher-agent-plugin-skill-directories :to-be nil)))

  (it "SKLP-02: discovers bundled skills, compact command, and pure replace_own_buffer tool"
    (let ((macher-agent-plugin-skill-directories nil)
          (macher-agent-skill-directories nil)
          (macher-agent-install-hook nil)
          (gptel--known-tools nil)
          (gptel--known-presets nil)
          (gptel-directives nil)
          (macher-agent-global-skills-alist nil)
          (macher-agent-tools-registry nil))
      (unwind-protect
          (progn
            (macher-agent-skills-install)
            (macher-agent-initialize-skills)
            (dolist (sym '(macher-agent-router
                           macher-agent-plan
                           macher-agent-synthesiser
                           macher-agent-worker
                           skill-creator
                           skill-converter
                           inkscape-multipage-presentation
                           compact))
              (expect (assq sym macher-agent-global-skills-alist) :to-be-truthy))
            (let ((tool (gptel-get-tool "replace_own_buffer")))
              (expect tool :to-be-truthy)
              (expect (get 'macher-agent-replace-own-buffer-tool 'ptc-function) :to-be nil)))
        (macher-agent-skills-uninstall))))

  (it "SKLP-03: replace_own_buffer replaces content, clears gptel--bounds, and clears wire buffer"
    (let ((macher-agent-plugin-skill-directories nil)
          (macher-agent-skill-directories nil)
          (macher-agent-install-hook nil)
          (gptel--known-tools nil)
          (gptel--known-presets nil)
          (gptel-directives nil)
          (macher-agent-global-skills-alist nil)
          (macher-agent-tools-registry nil)
          (wire-cleared-buf nil))
      (unwind-protect
          (progn
            (macher-agent-skills-install)
            (macher-agent-initialize-skills)
            (let ((tool (gptel-get-tool "replace_own_buffer")))
              (expect tool :to-be-truthy)
              (with-temp-buffer
                (insert "old long conversation text")
                (setq-local gptel--bounds '((response (1 20))))
                (setq-local gptel-post-response-functions nil)
                (let ((cb-result nil)
                      (this-buf (current-buffer)))
                  (cl-letf (((symbol-function 'macher-agent-wire-clear-buffer)
                             (lambda (&optional b) (setq wire-cleared-buf (or b (current-buffer)))))
                            ((symbol-function 'gptel-prompt-prefix-string)
                             (lambda () "### ")))
                    (funcall (gptel-tool-function tool)
                             (lambda (res) (setq cb-result res))
                             "Compacted state summary")
                    (expect cb-result :to-match "SUCCESS:")
                    (run-hook-with-args 'gptel-post-response-functions 1 10)
                    (expect (buffer-string) :to-equal "Compacted state summary\n\n### ")
                    (expect gptel--bounds :to-be nil)
                    (expect wire-cleared-buf :to-be this-buf))))))
        (macher-agent-skills-uninstall)))))

;;; test-skills-pack.el ends here
