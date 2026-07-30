;;; macher-agent-snippet-extractor.el --- Snippet extractor helper for tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Helper module for extracting and validating executable Emacs Lisp code snippets
;; from Markdown files using imenu index structure.

;;; Code:

(require 'imenu)
(require 'subr-x)

(defun macher-agent-test--flatten-imenu-index (index-alist)
  "Recursively flatten INDEX-ALIST into a list of (NAME . POSITION) pairs."
  (let ((results nil))
    (dolist (item index-alist)
      (when (consp item)
        (let ((name (car item))
              (val (cdr item)))
          (unless (or (not (stringp name))
                      (string-prefix-p "*" name))
            (cond
             ;; Case 1: (NAME . integer/marker/overlay)
             ((or (integerp val) (markerp val) (overlayp val))
              (let ((pos (cond ((integerp val) val)
                               ((markerp val) (marker-position val))
                               ((overlayp val) (overlay-start val)))))
                (when pos
                  (push (cons name pos) results))))
             ;; Case 2: (NAME POS ...) where POS is integer/marker/overlay
             ((and (listp val)
                   (car val)
                   (or (integerp (car val)) (markerp (car val)) (overlayp (car val))))
              (let* ((p (car val))
                     (pos (cond ((integerp p) p)
                                ((markerp p) (marker-position p))
                                ((overlayp p) (overlay-start p)))))
                (when pos
                  (push (cons name pos) results))))
             ;; Case 3: Submenu where val is a list of sub-items
             ((listp val)
              (setq results (nconc (nreverse (macher-agent-test--flatten-imenu-index val))
                                   results))))))))
    (nreverse results)))

(defun macher-agent-test--valid-lisp-code-p (code)
  "Return non-nil if CODE contains valid executable Emacs Lisp/Lisp forms."
  (when (and (stringp code)
             (not (string-empty-p (string-trim code)))
             (string-match-p "(" code))
    (condition-case nil
        (let ((pos 0)
              (len (length code))
              (has-lisp-form nil)
              (valid t))
          (while (and valid (< pos len))
            (cond
             ;; Skip whitespace
             ((string-match "\\`[ \t\n\r]+" (substring code pos))
              (setq pos (+ pos (match-end 0))))
             ;; Skip single-line comments
             ((string-match "\\`;[^\n\r]*" (substring code pos))
              (setq pos (+ pos (match-end 0))))
             (t
              (let ((res (read-from-string code pos)))
                (setq pos (cdr res))
                (let ((form (car res)))
                  (when (consp form)
                    (setq has-lisp-form t)))))))
          (and valid has-lisp-form))
      (error nil))))

(defun macher-agent-test--extract-snippets-in-region (start-pos end-pos)
  "Extract code snippets enclosed in markdown code fences or org babel between START-POS and END-POS."
  (let ((snippets nil)
        (case-fold-search t))
    (save-excursion
      (goto-char start-pos)
      (while (and (< (point) end-pos)
                  (re-search-forward "^[ \t]*\\(```\\|#\\+begin_src\\)" end-pos t))
        (let ((block-type (match-string 1)))
          (forward-line 1)
          (let ((code-start (point))
                (code-end nil))
            (if (string-prefix-p "#+" block-type)
                ;; Org babel syntax: search for #+end_src
                (when (re-search-forward "^[ \t]*#\\+end_src[ \t]*$" end-pos t)
                  (setq code-end (match-beginning 0))
                  (forward-line 1))
              ;; Markdown code fence syntax: search for closing ```
              (when (re-search-forward "^[ \t]*```[ \t]*$" end-pos t)
                (setq code-end (match-beginning 0))
                (forward-line 1)))
            (when (and code-end (<= code-start code-end))
              (let ((code (buffer-substring-no-properties code-start code-end)))
                (push code snippets)))))))
    (nreverse snippets)))

(defun macher-agent-test--extract-via-imenu (file-path)
  "Extract valid executable Emacs Lisp code snippets from FILE-PATH using imenu.
Returns a list of property lists: (:name \"Heading Name\" :code \"(code...)\" :file file-path)."
  (let* ((expanded (expand-file-name file-path))
         (existing-buf (find-buffer-visiting expanded))
         (buf (or existing-buf (find-file-noselect expanded))))
    (unwind-protect
        (with-current-buffer buf
          ;; Activate mode: markdown-ts-mode if available, else markdown-mode
          (cond
           ((fboundp 'markdown-ts-mode) (markdown-ts-mode))
           ((fboundp 'markdown-mode) (markdown-mode)))
          ;; Update Tree-Sitter ranges if available
          (when (fboundp 'treesit-update-ranges)
            (treesit-update-ranges))
          ;; Generate imenu index
          (let* ((index-alist (imenu--make-index-alist))
                 (flattened (macher-agent-test--flatten-imenu-index index-alist))
                 (sorted (sort flattened (lambda (a b) (< (cdr a) (cdr b)))))
                 (num-headings (length sorted))
                 (results nil))
            (dotimes (i num-headings)
              (let* ((heading (nth i sorted))
                     (heading-name (car heading))
                     (start-pos (cdr heading))
                     (end-pos (if (< (1+ i) num-headings)
                                  (cdr (nth (1+ i) sorted))
                                (point-max)))
                     (snippets (macher-agent-test--extract-snippets-in-region start-pos end-pos)))
                (dolist (snippet snippets)
                  (let ((trimmed (string-trim snippet)))
                    (when (macher-agent-test--valid-lisp-code-p trimmed)
                      (push (list :name heading-name
                                  :code trimmed
                                  :file file-path)
                            results))))))
            (nreverse results)))
      (unless existing-buf
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'macher-agent-snippet-extractor)
;;; macher-agent-snippet-extractor.el ends here
