;;; macher-agent-zero-mem-interactive.el --- Manual Tests for Macher Agent Zero-Mem -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Interactive and manual benchmarks and query inspection routines
;; for the macher-agent-zero-mem PageRank and graph retrieval engine.
;;

;;; Code:
;;; https://github.com/mohammadtavakoli78/BEAM/blob/main/chats/100K/1/chat.json
;;; https://arxiv.org/abs/2607.29377

(require 'macher-agent-zero-mem)
(require 'json)

;; 1. Environment isolation
(defun macher-agent-zero-mem-test-isolate-environment ()
  "Isolate test environment before each benchmark run."
  (when (boundp 'timer-list)
    (mapc #'cancel-timer (copy-sequence timer-list)))
  (when (boundp 'timer-idle-list)
    (mapc #'cancel-timer (copy-sequence timer-idle-list)))
  (dolist (buf (buffer-list))
    (let ((bname (buffer-name buf)))
      (when bname
        (unless (or (eq buf (current-buffer))
                    (member bname '("*scratch*" "*Messages*" "*stdout*" "*stderr*"))
                    (string-prefix-p " *" bname)
                    (string-match-p "Macher Intensive" bname))
          (ignore-errors (kill-buffer buf))))))
  (garbage-collect))

;; 2. Native metrics extraction
(defun macher-agent-zero-mem-test-measure-execution (thunk)
  "Execute THUNK and extract benchmarking metrics."
  (let* ((gc-cycles-before gcs-done)
         (gc-time-before gc-elapsed)
         (start-time (float-time))
         (result (funcall thunk))
         (end-time (float-time))
         (gc-cycles-after gcs-done)
         (gc-time-after gc-elapsed))
    (list :result result
          :total-time (- end-time start-time)
          :gc-cycles (- gc-cycles-after gc-cycles-before)
          :gc-time (- gc-time-after gc-time-before))))

;; 3. Naive regular expression tool
(defun macher-agent-zero-mem-test-naive-glob (query target-buffer ctx-lines)
  "Extract lines matching QUERY from TARGET-BUFFER with CTX-LINES context."
  (let ((results nil))
    (with-current-buffer target-buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward query nil t)
          (let* ((match-pt (point))
                 (start-pt (save-excursion (forward-line (- ctx-lines)) (line-beginning-position)))
                 (end-pt (save-excursion (forward-line ctx-lines) (line-end-position))))
            (push (buffer-substring-no-properties start-pt end-pt) results)))))
    (if results (string-join (nreverse results) "\n") "")))

;; 4. Shared corpus
(defun macher-agent-zero-mem-load-corpus-from-disk (filepath target-buffer limit)
  "Load JSON trace data from FILEPATH into TARGET-BUFFER up to LIMIT.
Extracts content from user and assistant roles and processes it line by line.
Returns a list of raw trace plists for the graph engine."
  (let ((traces nil)
        (id-counter 0))
    (with-current-buffer target-buffer
      (erase-buffer))

    ;; Insert file into a temporary buffer to use the fast native C parser
    (let ((data (with-temp-buffer
                  (insert-file-contents filepath)
                  (goto-char (point-min))
                  (json-parse-buffer :object-type 'alist :array-type 'list))))

      (catch 'limit-reached
        ;; Iterate through the root array of batch objects
        (dolist (batch data)
          (let ((turns (alist-get 'turns batch)))

            ;; Iterate through the conversational turns
            (dolist (turn turns)
              (dolist (message turn)
                (let ((role (alist-get 'role message))
                      (content (alist-get 'content message)))

                  ;; Isolate the user and assistant roles
                  (when (and (stringp role)
                             (stringp content)
                             (or (string= role "user") (string= role "assistant")))

                    ;; Split the content block into individual lines
                    (dolist (line (split-string content "\n"))
                      (let ((trimmed-line (string-trim line)))

                        ;; Ignore empty lines to prevent blank nodes in the graph
                        (unless (string-empty-p trimmed-line)
                          (when (>= id-counter limit)
                            (throw 'limit-reached t))

                          (let ((trace (list :text trimmed-line
                                             :timestamp (float id-counter)
                                             :metadata (list :source filepath
                                                             :role role
                                                             :index id-counter))))

                            ;; Insert the line into the buffer for the naive glob
                            (with-current-buffer target-buffer
                              (insert trimmed-line "\n"))

                            ;; Store the trace for the graph engine
                            (push trace traces)
                            (setq id-counter (1+ id-counter))))))))))))))

    (nreverse traces)))

;; 5. The interactive benchmark command
(defun macher-agent-zero-mem-intensive-benchmark (param)
  "Run intensive memory and execution benchmarks using PARAM interactively across corpus sizes."
  (interactive "sQuery: ")
  (let ((sizes '(1000 5000 10000))
        (query param)
        (results-buf (get-buffer-create "*Macher Intensive Benchmarks*")))

    (with-current-buffer results-buf
      (erase-buffer)
      (insert "Macher Agent Zero-Mem Intensive Benchmarks\n")
      (insert "==========================================\n\n"))

    (dolist (size sizes)
      (let* ((test-buf (generate-new-buffer " *macher-test-heavy*"))
             (traces (macher-agent-zero-mem-load-corpus-from-disk "chat.json" test-buf size))
             (graph (macher-agent-zero-mem-build-graph traces)))

        (with-current-buffer results-buf
          (insert (format "Corpus Size: %d Traces\n" size)
                  "--------------------------------------------------\n"))

        ;; Measure Float PPR
        (macher-agent-zero-mem-test-isolate-environment)
        (let* ((float-metrics (macher-agent-zero-mem-test-measure-execution
                               (lambda () (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 15 :algorithm 'float))))
               (ft (plist-get float-metrics :total-time))
               (fgc (plist-get float-metrics :gc-cycles))
               (fgctime (plist-get float-metrics :gc-time)))
          (with-current-buffer results-buf
            (insert (format "Float PPR       | Time: %9.6f s | GC Cycles: %3d | GC Time: %9.6f s\n" ft fgc fgctime))))

        ;; Measure Fixed-Point PPR
        (macher-agent-zero-mem-test-isolate-environment)
        (let* ((fp-metrics (macher-agent-zero-mem-test-measure-execution
                            (lambda () (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 15 :algorithm 'fixed-point))))
               (fpt (plist-get fp-metrics :total-time))
               (fpgc (plist-get fp-metrics :gc-cycles))
               (fpgctime (plist-get fp-metrics :gc-time)))
          (with-current-buffer results-buf
            (insert (format "Fixed-Point PPR | Time: %9.6f s | GC Cycles: %3d | GC Time: %9.6f s\n" fpt fpgc fpgctime))))

        ;; Measure Naive Glob
        (macher-agent-zero-mem-test-isolate-environment)
        (let* ((glob-metrics (macher-agent-zero-mem-test-measure-execution
                              (lambda () (macher-agent-zero-mem-test-naive-glob query test-buf 5))))
               (gt (plist-get glob-metrics :total-time))
               (ggc (plist-get glob-metrics :gc-cycles)))
          (with-current-buffer results-buf
            (insert (format "Naive Glob      | Time: %9.6f s | GC Cycles: %3d\n" gt ggc))))

        (with-current-buffer results-buf
          (insert "\n"))

        (kill-buffer test-buf)))

    (pop-to-buffer results-buf)))

;; 6. Interactive query inspection
(defun macher-agent-zero-mem-inspect-query (query limit)
  "Run a single QUERY against the disk corpus up to LIMIT and show all results."
  (interactive "sQuery: \nnNumber of traces to load (for example 1000): ")
  (let* ((test-buf (generate-new-buffer " *macher-test-inspect*"))
         (results-buf (get-buffer-create "*Macher Query Inspection*"))
         (traces (macher-agent-zero-mem-load-corpus-from-disk "chat.json" test-buf limit))
         (graph (macher-agent-zero-mem-build-graph traces)))

    (with-current-buffer results-buf
      (erase-buffer)
      (insert "Macher Agent Zero-Mem Query Inspection\n")
      (insert "======================================\n")
      (insert "Query: " query "\n")
      (insert "Corpus Size: " (number-to-string limit) " Traces\n\n")

      ;; 1. Naive regular expression glob
      (insert "1. Naive Buffer Glob\n")
      (insert "--------------------\n")
      (let ((glob-res (macher-agent-zero-mem-test-naive-glob query test-buf 2)))
        (if (string-empty-p glob-res)
            (insert "No matches found.\n\n")
          (insert glob-res "\n\n")))

      (cl-flet ((format-traces (trace-list)
                  (if (null trace-list)
                      "No traces retrieved.\n"
                    (string-join
                     (mapcar (lambda (t-obj)
                               (format "[Trace %d] %s"
                                       (macher-agent-zero-mem-trace-id t-obj)
                                       (macher-agent-zero-mem-trace-text t-obj)))
                             trace-list)
                     "\n"))))

        ;; 2. Dual-view graph with float PageRank
        (insert "2. Dual-View Graph (Float PPR)\n")
        (insert "------------------------------\n")
        (let ((float-res (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 15 :algorithm 'float)))
          (insert (format-traces float-res) "\n\n"))

        ;; 3. Dual-view graph with fixed-point PageRank
        (insert "3. Dual-View Graph (Fixed-Point PPR)\n")
        (insert "------------------------------------\n")
        (let ((fp-res (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 15 :algorithm 'fixed-point)))
          (insert (format-traces fp-res) "\n\n"))))

    (kill-buffer test-buf)
    (pop-to-buffer results-buf)))

(provide 'macher-agent-zero-mem-interactive)
;;; macher-agent-zero-mem-interactive.el ends here
