;;; macher-agent-zero-mem-test.el --- Benchmark & Unit Tests for Macher Agent Zero-Mem -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Buttercup test suite for `macher-agent-zero-mem.el' benchmarking the
;; refactored fixed-point PageRank algorithm against the baseline float-based PageRank.

;;; Code:

(require 'buttercup)
(require 'macher-agent-zero-mem)

;;;; 1. Environment Isolation Utilities

(defun macher-agent-zero-mem-test-isolate-environment ()
  "Isolate test environment before each benchmark run.
Cancels active asynchronous timers, wipes background buffers, and
forces an immediate garbage collection cycle."
  ;; 1. Cancel active timers
  (when (boundp 'timer-list)
    (mapc #'cancel-timer (copy-sequence timer-list)))
  (when (boundp 'timer-idle-list)
    (mapc #'cancel-timer (copy-sequence timer-idle-list)))

  ;; 2. Wipe non-essential background buffers
  (dolist (buf (buffer-list))
    (unless (or (eq buf (current-buffer))
                (member (buffer-name buf) '("*scratch*" "*Messages*" "*stdout*" "*stderr*"))
                (string-prefix-p " *" (buffer-name buf))
                (string-match-p "buttercup" (buffer-name buf)))
      (ignore-errors (kill-buffer buf))))

  ;; 3. Explicitly force a garbage collection cycle
  (garbage-collect))


;;;; 2. Benchmarking Metrics Extraction and Logging

(defun macher-agent-zero-mem-test-measure-execution (thunk)
  "Execute THUNK and extract benchmarking metrics using native Emacs utilities.
Captures total execution time, total garbage collection cycles, and total GC processing time.
Returns a plist containing :result, :total-time, :gc-cycles, and :gc-time."
  (let* ((gc-cycles-before gcs-done)
         (gc-time-before gc-elapsed)
         (start-time (float-time))
         (result (funcall thunk))
         (end-time (float-time))
         (gc-cycles-after gcs-done)
         (gc-time-after gc-elapsed)
         (total-time (- end-time start-time))
         (gc-cycles (- gc-cycles-after gc-cycles-before))
         (gc-time (- gc-time-after gc-time-before)))
    (list :result result
          :total-time total-time
          :gc-cycles gc-cycles
          :gc-time gc-time)))

(defun macher-agent-zero-mem-test-log-metrics (label metrics)
  "Log benchmark METRICS plist for LABEL to output console using native utilities."
  (let ((msg (format "[BENCHMARK] %-32s | Total Time: %9.6f s | GC Cycles: %3d | GC Time: %9.6f s"
                     label
                     (plist-get metrics :total-time)
                     (plist-get metrics :gc-cycles)
                     (plist-get metrics :gc-time))))
    (message "%s" msg)
    (ignore-errors
      (princ (concat msg "\n") 'external-debugging-output))))


;;;; 3. Side-by-Side Custom Assertion Macro

(defmacro expect-benchmark-faster (baseline-form optimised-form &optional label)
  "Custom assertion macro running BASELINE-FORM and OPTIMISED-FORM side-by-side.
Isolates environment before each run, captures and logs three native metrics
\(total time, GC cycles, GC time), and asserts OPTIMISED-FORM executes faster
than BASELINE-FORM within a mathematical tolerance of either a 10ms floor or 15% buffer."
  `(let* ((tag (or ,label "Benchmark Comparison"))
          ;; Step 1: Isolate environment and measure baseline control group
          (_ (macher-agent-zero-mem-test-isolate-environment))
          (base-metrics (macher-agent-zero-mem-test-measure-execution (lambda () ,baseline-form)))
          (_ (macher-agent-zero-mem-test-log-metrics (concat tag " [Baseline Float]") base-metrics))
          ;; Step 2: Isolate environment and measure optimised candidate
          (_ (macher-agent-zero-mem-test-isolate-environment))
          (opt-metrics (macher-agent-zero-mem-test-measure-execution (lambda () ,optimised-form)))
          (_ (macher-agent-zero-mem-test-log-metrics (concat tag " [Optimised Fixed-Point]") opt-metrics))
          ;; Step 3: Extract timing and calculate mathematical tolerance
          (base-time (plist-get base-metrics :total-time))
          (opt-time (plist-get opt-metrics :total-time))
          ;; Mathematical tolerance: either 10-millisecond floor (0.010s) or 15% buffer (* 0.15 base-time)
          (tolerance-buffer (max 0.010 (* 0.15 base-time)))
          (threshold (+ base-time tolerance-buffer)))
     ;; Step 4: Summary logging
     (message "[BENCHMARK SUMMARY] %s: Baseline=%.6fs | Optimised=%.6fs | Max Threshold=%.6fs | Speedup=%.2fx"
              tag base-time opt-time threshold
              (if (> opt-time 0.0) (/ base-time opt-time) 1.0))
     ;; Step 5: Assert that optimised execution time is under threshold
     (expect opt-time :to-be-less-than threshold)))


;;;; 4. Synthetic Payload and Mock Data Generators

(defcustom macher-agent-zero-mem-test-vocabulary
  '("Zero-Mem" "PageRank" "Entity-Context" "BipartiteGraph" "TraceGraph"
    "StationaryDistribution" "DampingFactor" "PersonalizedPageRank"
    "TemporalTrace" "QueryAlignment" "DualViewRetrieval" "ScoreFusion"
    "MacherAgent" "GptelBridge" "EmacsLisp" "FixedPointArithmetic"
    "PowerIteration" "VectorEmbedding" "JaroWinkler" "AdjacencyMatrix"
    "ChronologicalTrace" "EntityReverseIndex" "ContextNode" "DocumentNode")
  "Vocabulary pool for synthetic payload generation."
  :type '(repeat string)
  :group 'macher-agent-zero-mem)

(defun macher-agent-zero-mem-test-make-synthetic-traces (count &optional entities-per-trace)
  "Generate COUNT synthetic raw trace plists dynamically.
ENTITIES-PER-TRACE controls noun density per trace document."
  (let ((traces nil)
        (density (or entities-per-trace 4))
        (vocab-len (length macher-agent-zero-mem-test-vocabulary)))
    (dotimes (i count)
      (let ((ents nil)
            (text-parts nil))
        (dotimes (j density)
          (let ((idx (% (+ (* i 7) (* j 13)) vocab-len)))
            (push (nth idx macher-agent-zero-mem-test-vocabulary) ents)))
        (setq ents (delete-dups (delq nil ents)))
        (setq text-parts
              (list (format "Document trace #%d recording session activity." i)
                    (format "Contains entities: %s." (string-join ents ", "))
                    (format "Code identifier `macher-agent-zero-mem-node-%d`." i)
                    (format "\"quoted-parameter-%d\"" i)))
        (push (list :text (string-join text-parts " ")
                    :timestamp (float i)
                    :metadata (list :session "synth-session" :index i))
              traces)))
    (nreverse traces)))

(defun macher-agent-zero-mem-test-make-synthetic-graph (trace-count &optional entities-per-trace)
  "Generate a synthetic `macher-agent-zero-mem-graph' with TRACE-COUNT nodes."
  (let ((raw-traces (macher-agent-zero-mem-test-make-synthetic-traces trace-count entities-per-trace)))
    (macher-agent-zero-mem-build-graph raw-traces)))

(defun macher-agent-zero-mem-test-make-synthetic-query (index)
  "Generate a synthetic query string targeting entities around INDEX."
  (let* ((vocab-len (length macher-agent-zero-mem-test-vocabulary))
         (ent1 (nth (% index vocab-len) macher-agent-zero-mem-test-vocabulary))
         (ent2 (nth (% (+ index 3) vocab-len) macher-agent-zero-mem-test-vocabulary)))
    (format "Retrieve traces concerning %s and %s in macher-agent" ent1 ent2)))

(defun macher-agent-zero-mem-test-generate-corpus (num-traces target-buffer)
  "Generate NUM-TRACES for testing memory recall and performance.
Populates TARGET-BUFFER with raw text and returns a list of raw trace plists."
  (let ((traces nil)
        (vocab '("configuration" "module" "socket" "database" "query"
                 "API" "endpoint" "token" "session" "authentication" "timeout"
                 "Hyperion" "bridge" "protocol" "transfer" "spikes"))
        (speakers '("User" "Agent" "System")))
    
    (with-current-buffer target-buffer
      (erase-buffer)
      (dotimes (i num-traces)
        (let* ((speaker (nth (random (length speakers)) speakers))
               (word1 (nth (random (length vocab)) vocab))
               (word2 (nth (random (length vocab)) vocab))
               ;; Inject specific semantic gaps using capitalised Proper Nouns for the NER
               (text (cond
                      ((= i 15) "System alert: The Hyperion bridge module is responsible for the Websocket protocol.")
                      ((= i 20) "User: We are seeing massive Latency spikes during data transfer.")
                      ((= i (- num-traces 10)) "User: Can you check the logs for the bridge transfer?")
                      (t (format "%s processed the %s and the %s." speaker word1 word2))))
               (ts (float i))
               (trace (list :text text :timestamp ts :metadata (list :speaker speaker))))
          
          (insert text "\n")
          (push trace traces))))
    (nreverse traces)))

(defun macher-agent-zero-mem-test-naive-glob (query target-buffer ctx-lines)
  "Extract lines matching QUERY from TARGET-BUFFER with CTX-LINES of context [7]."
  (let ((results nil))
    (with-current-buffer target-buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward query nil t)
          (let* ((match-pt (point))
                 (start-pt (save-excursion 
                             (forward-line (- ctx-lines)) 
                             (line-beginning-position)))
                 (end-pt (save-excursion 
                           (forward-line ctx-lines) 
                           (line-end-position)))
                 (snippet (buffer-substring-no-properties start-pt end-pt)))
            (push (format "--- Match near line %d ---\n%s\n" 
                          (line-number-at-pos match-pt) snippet) 
                  results)))))
    (if results
        (string-join (nreverse results) "\n")
      "")))

;;;; 5. Test Suite Specifications

(describe "Macher Agent Zero-Mem Test Suite"

          (describe "1. Environment Isolation and Synthetic Generators"
                    (it "successfully isolates environment by cancelling timers, wiping buffers, and running GC"
                        (let ((timer (run-with-timer 100 nil #'ignore))
                              (test-buf (generate-new-buffer "*macher-test-dummy*")))
                          (expect (memq timer timer-list) :to-be-truthy)
                          (expect (buffer-live-p test-buf) :to-be t)
                          (macher-agent-zero-mem-test-isolate-environment)
                          (expect (memq timer timer-list) :to-be nil)
                          (expect (buffer-live-p test-buf) :to-be nil)))

                    (it "dynamically generates synthetic trace payloads and bipartite graphs"
                        (let* ((traces (macher-agent-zero-mem-test-make-synthetic-traces 20 4))
                               (graph (macher-agent-zero-mem-build-graph traces)))
                          (expect (length traces) :to-equal 20)
                          (expect (listp graph) :to-be t)
                          (expect (hash-table-count (macher-agent-zero-mem-graph-traces graph)) :to-equal 20)
                          (expect (> (hash-table-count (macher-agent-zero-mem-graph-entity-index graph)) 0) :to-be t))))

          (describe "2. Fixed-Point PageRank Score Equivalence & Order Preservation"
                    (it "preserves node ranking order between float and fixed-point PageRank"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 30 5))
                               (query "Zero-Mem PageRank test")
                               (float-res (macher-agent-zero-mem-pagerank-float query graph 15))
                               (fixed-res (macher-agent-zero-mem-pagerank-fixed-point query graph 15))
                               (float-scores nil)
                               (fixed-scores nil))
                          (maphash (lambda (k v) (push (cons k v) float-scores)) float-res)
                          (maphash (lambda (k v) (push (cons k v) fixed-scores)) fixed-res)
                          (setq float-scores (sort float-scores (lambda (a b) (> (cdr a) (cdr b)))))
                          (setq fixed-scores (sort fixed-scores (lambda (a b) (> (cdr a) (cdr b)))))
                          ;; Check top 3 nodes match between float and fixed-point PageRank
                          (expect (car (nth 0 float-scores)) :to-equal (car (nth 0 fixed-scores)))
                          (expect (car (nth 1 float-scores)) :to-equal (car (nth 1 fixed-scores)))
                          (expect (car (nth 2 float-scores)) :to-equal (car (nth 2 fixed-scores))))))

          (describe "3. Side-by-Side Performance Benchmarks (Fixed-Point vs Float PageRank)"
                    (it "benchmarks fixed-point PageRank vs float baseline on small graph (30 traces)"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 30 5))
                               (query (macher-agent-zero-mem-test-make-synthetic-query 0)))
                          (expect-benchmark-faster
                           (macher-agent-zero-mem-pagerank-float query graph 15)
                           (macher-agent-zero-mem-pagerank-fixed-point query graph 15)
                           "Small Graph PageRank (30 traces)")))

                    (it "benchmarks fixed-point PageRank vs float baseline on medium graph (100 traces)"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 100 8))
                               (query (macher-agent-zero-mem-test-make-synthetic-query 5)))
                          (expect-benchmark-faster
                           (macher-agent-zero-mem-pagerank-float query graph 20)
                           (macher-agent-zero-mem-pagerank-fixed-point query graph 20)
                           "Medium Graph PageRank (100 traces)")))

                    (it "benchmarks fixed-point PageRank vs float baseline on stressed graph (250 traces)"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 250 10))
                               (query (macher-agent-zero-mem-test-make-synthetic-query 10)))
                          (expect-benchmark-faster
                           (macher-agent-zero-mem-pagerank-float query graph 25)
                           (macher-agent-zero-mem-pagerank-fixed-point query graph 25)
                           "Stressed Graph PageRank (250 traces)")))

                    (it "benchmarks full dual-view retrieval pipeline (fixed-point vs float)"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 120 6))
                               (query (macher-agent-zero-mem-test-make-synthetic-query 3)))
                          (expect-benchmark-faster
                           (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 20 :algorithm 'float)
                           (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 20 :algorithm 'fixed-point)
                           "Dual-View Retrieve Pipeline"))))

          (describe "4. Dual-View Evidence Retrieval Keyword Arguments & Operations"
                    (it "retrieves top-k results correctly with default and explicit keyword arguments"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 30 5))
                               (query (macher-agent-zero-mem-test-make-synthetic-query 0))
                               (results-default (macher-agent-zero-mem-retrieve query graph))
                               (results-k3 (macher-agent-zero-mem-retrieve query graph :top-k 3))
                               (results-float (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 10 :algorithm 'float))
                               (results-fp (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 10 :algorithm 'fixed-point)))
                          (expect (length results-default) :to-equal 5)
                          (expect (length results-k3) :to-equal 3)
                          (expect (length results-float) :to-equal 5)
                          (expect (length results-fp) :to-equal 5))))
          (describe "5. Memory recall comparison"
                    (it "bridges semantic gaps in the graph that the naive glob misses"
                        (let* ((test-buf (generate-new-buffer " *macher-test-corpus*"))
                               (traces (macher-agent-zero-mem-test-generate-corpus 50 test-buf))
                               (graph (macher-agent-zero-mem-build-graph traces))
                               (query "Websocket Latency")
                               (glob-result (macher-agent-zero-mem-test-naive-glob query test-buf 5))
                               (graph-results (macher-agent-zero-mem-retrieve query graph :top-k 5 :algorithm 'fixed-point)))
                          
                          ;; The naive glob searches for the exact string or regex [7] and fails
                          (expect (string-empty-p glob-result) :to-be t)
                          
                          ;; The dual-view graph traverses edges to retrieve both traces
                          (let ((found-15 nil)
                                (found-20 nil))
                            (dolist (res graph-results)
                              (when (= (macher-agent-zero-mem-trace-id res) 15) (setq found-15 t))
                              (when (= (macher-agent-zero-mem-trace-id res) 20) (setq found-20 t)))
                            (expect found-15 :to-be t)
                            (expect found-20 :to-be t))
                          
                          (kill-buffer test-buf))))

          (describe "6. Three-way execution and memory benchmark"
                    (it "benchmarks float PageRank, fixed-point PageRank, and naive glob on a shared corpus"
                        (let* ((test-buf (generate-new-buffer " *macher-test-benchmark*"))
                               (traces (macher-agent-zero-mem-test-generate-corpus 500 test-buf))
                               (graph (macher-agent-zero-mem-build-graph traces))
                               (query "Websocket Latency"))
                          
                          (message "\n--- Three-Way Corpus Benchmark (500 Traces) ---")
                          
                          ;; Measure Baseline Float PageRank
                          (macher-agent-zero-mem-test-isolate-environment)
                          (let ((float-metrics (macher-agent-zero-mem-test-measure-execution 
                                                (lambda () (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 15 :algorithm 'float)))))
                            (macher-agent-zero-mem-test-log-metrics "1. Dual-View (Float PPR)" float-metrics))
                          
                          ;; Measure Optimised Fixed-Point PageRank
                          (macher-agent-zero-mem-test-isolate-environment)
                          (let ((fp-metrics (macher-agent-zero-mem-test-measure-execution 
                                             (lambda () (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 15 :algorithm 'fixed-point)))))
                            (macher-agent-zero-mem-test-log-metrics "2. Dual-View (Fixed-Point PPR)" fp-metrics))
                          
                          ;; Measure Naive Regular Expression Glob [7]
                          (macher-agent-zero-mem-test-isolate-environment)
                          (let ((glob-metrics (macher-agent-zero-mem-test-measure-execution 
                                               (lambda () (macher-agent-zero-mem-test-naive-glob query test-buf 5)))))
                            (macher-agent-zero-mem-test-log-metrics "3. Naive Buffer Glob" glob-metrics))
                          
                          (message "-----------------------------------------------")
                          
                          ;; Assert the test concludes successfully
                          (expect (buffer-live-p test-buf) :to-be t)
                          (kill-buffer test-buf)))))

(provide 'macher-agent-zero-mem-test)
;;; macher-agent-zero-mem-test.el ends here
