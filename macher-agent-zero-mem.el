;;; macher-agent-zero-mem.el --- Zero-Token Memory Operations for macher-agent -*- lexical-binding: t; -*-


;;; Commentary:
;;
;; This is a self-contained, Emacs-native implementation of the Zero-Mem
;;
;; It features:
;; 1. A Naive deterministic Elisp Named Entity Recognition (NER) extractor.
;; 2. Bipartite Entity-Context Graph construction with chronological trace adjacency.
;; 3. Stationary Personalised PageRank (PPR) diffusion mathematically modeled in Lisp.
;; 4. Chronological/Temporal trace indexing and multi-granular retrieval.
;; 5. Score fusion and query-conditioned routing.

;;; Code:

(require 'cl-lib)

(defgroup macher-agent-zero-mem nil
  "Zero-Token Memory Operations for LLM agents."
  :group 'macher-agent
  :prefix "macher-agent-zero-mem-")

(defcustom macher-agent-zero-mem-damping-factor 0.6
  "The damping factor (gamma) for Personalised PageRank diffusion."
  :type 'float
  :group 'macher-agent-zero-mem)

(defcustom macher-agent-zero-mem-entity-transition-ratio 0.7
  "Ratio of document transitions allocated to entities vs temporal
neighbors (E_dd)."
  :type 'float
  :group 'macher-agent-zero-mem)

(defcustom macher-agent-zero-mem-stop-words
  '("The" "This" "That" "They" "Here" "There" "What" "When" "How" "Why" "Who"
    "But" "And" "For" "With" "From" "Your" "I" "We" "You" "It" "An" "If" "Or"
    "As" "To" "In" "At" "Of" "On" "By" "About" "An" "Be" "Is" "Are" "Was" "Were"
    "Been" "Has" "Have" "Had" "Do" "Does" "Did" "Will" "Would" "Should" "Can")
  "List of capitalization-prone common stop words to filter out during NER."
  :type '(repeat string)
  :group 'macher-agent-zero-mem)


;;;; 1. Naive Named Entity Recognition (NER)

(defun macher-agent-zero-mem-naive-ner (text)
  "Extract entity strings from TEXT using deterministic heuristic rules [28].
Returns a deduplicated list of lowercase strings representing the entities."
  (let ((case-fold-search nil) ; Case-sensitive matching
        (entities nil))
    (with-temp-buffer
      (insert text)

      ;; Heuristic 1: Multi-word/Single Proper Nouns)
      (goto-char (point-min))
      (while (re-search-forward "\\b\\([A-Z][a-zA-Z0-9-]+\\(?: [A-Z][a-zA-Z0-9-]+\\)*\\)\\b" nil t)
        (let ((match (match-string 1)))
          (unless (member match macher-agent-zero-mem-stop-words)
            (push (downcase match) entities))))

      ;; Heuristic 2: Technical/Code Identifiers
      ;; Targets: kebab-case (macher-agent), snake_case (gptel_bridge), CamelCase (PageRank)
      (goto-char (point-min))
      (while (re-search-forward "\\b\\([a-z]+-[a-z0-9-]+\\|[a-z]+_[a-z0-9_]+\\|[A-Z][A-Za-z0-9_]+[a-z][A-Za-z0-9_]*\\)\\b" nil t)
        (push (downcase (match-string 1)) entities))

      ;; Heuristic 3: Exact Quoted Strings (indicating parameters, titles, files)
      (goto-char (point-min))
      (while (re-search-forward "\"\\([^\"]+\\)\"" nil t)
        (push (downcase (match-string 1)) entities)))

    (delete-dups entities)))


;;;; 2. Bipartite Graph Construction and Weighting

(cl-defstruct (macher-agent-zero-mem-trace
               (:constructor macher-agent-zero-mem-trace-create)
               (:type list))
  "Represents an individual context unit/document node (V_d) [29, 31]."
  id         ; Integer unique ID
  text       ; Raw string content [28]
  timestamp  ; Float time or integer sequence
  entities   ; List of extracted entity strings
  metadata)  ; Extensible plist (e.g., :session, :speaker)

(cl-defstruct (macher-agent-zero-mem-graph
               (:constructor macher-agent-zero-mem-graph-create)
               (:type list))
  "Represents the bipartite relational trace graph [29]."
  traces        ; Hash table: ID -> `macher-agent-zero-mem-trace'
  entity-index  ; Hash table: Entity -> List of trace IDs containing it (inverse index)
  adj-list)     ; Hash table: Node -> List of (NeighborNode . EdgeWeight)

(defun macher-agent-zero-mem-build-graph (traces-raw)
  "Build the relational trace graph from a list of plist raw traces TRACES-RAW.
TRACES-RAW should be a list of plists
example (:text \"text\" :timestamp 1.0 :metadata plist).
Returns a `macher-agent-zero-mem-graph' struct."
  (let* ((traces (make-hash-table :test 'equal))
         (entity-index (make-hash-table :test 'equal))
         (adj-list (make-hash-table :test 'equal))
         (trace-list nil)
         (id-counter 0))

    ;; Step 1: Instantiate V_d nodes and extract entities
    (dolist (raw traces-raw)
      (let* ((text (plist-get raw :text))
             (ts (or (plist-get raw :timestamp) (float id-counter)))
             (meta (plist-get raw :metadata))
             (ents (macher-agent-zero-mem-naive-ner text))
             (trace (macher-agent-zero-mem-trace-create
                     :id id-counter
                     :text text
                     :timestamp ts
                     :entities ents
                     :metadata meta)))
        (puthash id-counter trace traces)
        (push trace trace-list)
        (setq id-counter (1+ id-counter))))

    (setq trace-list (nreverse trace-list))

    ;; Step 2: Build the Entity Reverse Index (V_e)
    (dolist (trace trace-list)
      (let ((tid (macher-agent-zero-mem-trace-id trace)))
        (dolist (ent (macher-agent-zero-mem-trace-entities trace))
          (puthash ent (cons tid (gethash ent entity-index)) entity-index))))

    ;; Step 3: Populate Adjacency Weights (E_de & E_dd) [29]
    (let ((n-traces (length trace-list)))
      (dolist (trace trace-list)
        (let* ((tid (macher-agent-zero-mem-trace-id trace))
               (ents (macher-agent-zero-mem-trace-entities trace))
               (e-weight-sum (float (length ents)))
               (doc-node (cons :doc tid))
               (doc-transitions nil))

          ;; Transition from Document to Entities (E_de)
          (when (> e-weight-sum 0.0)
            (dolist (ent ents)
              ;; w(d_i, e) [Equation 4] is c(e, d_i)/sum(c(e', d_i)).
              ;; For naive NER, occurrences are binary, so weight is 1.0 / count(entities)
              (let* ((raw-w (/ 1.0 e-weight-sum))
                     (scaled-w (* macher-agent-zero-mem-entity-transition-ratio raw-w)))
                (push (cons (cons :ent ent) scaled-w) doc-transitions))))

          ;; Transition to Chronological Neighbors (E_dd) [29, 30]
          (let ((neighbors nil))
            (when (> tid 0) (push (1- tid) neighbors))
            (when (< tid (1- n-traces)) (push (1+ tid) neighbors))
            (when neighbors
              (let* ((temp-ratio (- 1.0 (if (null ents) 0.0 macher-agent-zero-mem-entity-transition-ratio)))
                     (temp-w (/ temp-ratio (float (length neighbors)))))
                (dolist (neigh-id neighbors)
                  (push (cons (cons :doc neigh-id) temp-w) doc-transitions)))))

          (puthash doc-node doc-transitions adj-list)))

      ;; Transition from Entities back to Documents (E_de reverse)
      (maphash
       (lambda (ent tids)
         (let* ((ent-node (cons :ent ent))
                (n-tids (float (length tids)))
                (ent-transitions nil))
           (dolist (tid tids)
             ;; Uniform transition back to documents that contain the entity [35]
             (push (cons (cons :doc tid) (/ 1.0 n-tids)) ent-transitions))
           (puthash ent-node ent-transitions adj-list)))
       entity-index))

    (macher-agent-zero-mem-graph-create
     :traces traces
     :entity-index entity-index
     :adj-list adj-list)))


;;;; 3. Query Alignment & PageRank Diffusion

(defun macher-agent-zero-mem-align-query (query graph)
  "Extract and align query entities with the GRAPH's Entity nodes [35].
Returns an association list of (EntityNode . ActivationScore)."
  (let* ((q-entities (macher-agent-zero-mem-naive-ner query))
         (aligned-activations nil))
    (dolist (q-ent q-entities)
      (when q-ent ; Safeguard against nil query entities
        (if (gethash q-ent (macher-agent-zero-mem-graph-entity-index graph))
            (push (cons (cons :ent q-ent) 1.0) aligned-activations)
          (maphash
           (lambda (g-ent _tids)
             ;; Safeguard against corrupted nil keys in the graph index
             (when (and g-ent 
                        (or (string-match-p (regexp-quote q-ent) g-ent)
                            (string-match-p (regexp-quote g-ent) q-ent)))
               (push (cons (cons :ent g-ent) 0.8) aligned-activations)))
           (macher-agent-zero-mem-graph-entity-index graph)))))
    aligned-activations))

(defun macher-agent-zero-mem-pagerank-float (query graph &optional iterations)
  "Compute the Stationary Personalised PageRank distribution over the GRAPH [37].
ITERATIONS defaults to 15. Returns a hash table mapping nodes to their PageRank
score.
Argument QUERY criteria."
  (let* ((iters (or iterations 15))
         (gamma macher-agent-zero-mem-damping-factor)
         (pi-table (make-hash-table :test 'equal))
         (reset-vector (make-hash-table :test 'equal))
         (nodes nil)
         (adj-list (macher-agent-zero-mem-graph-adj-list graph)))

    ;; 1. Collect all valid nodes
    (maphash (lambda (node _trans) (push node nodes)) adj-list)

    ;; 2. Establish the normalised Reset Distribution r_q [37]
    (let* ((alignments (macher-agent-zero-mem-align-query query graph))
           (align-sum (cl-loop for (_node . val) in alignments sum val)))
      (if (> align-sum 0.0)
          ;; Distribute reset probability over aligned query entity nodes
          (dolist (align alignments)
            (puthash (car align) (/ (cdr align) align-sum) reset-vector))
        ;; Fallback: uniform reset distribution over all Document nodes
        (let* ((doc-count 0)
               (doc-nodes nil))
          (dolist (node nodes)
            (when (eq (car node) :doc)
              (push node doc-nodes)
              (setq doc-count (1+ doc-count))))
          (if (> doc-count 0)
              (dolist (dn doc-nodes)
                (puthash dn (/ 1.0 (float doc-count)) reset-vector))
            ;; Ultimate fallback: absolute uniform over all nodes
            (let ((uniform-prob (/ 1.0 (float (length nodes)))))
              (dolist (node nodes) (puthash node uniform-prob reset-vector)))))))

    ;; 3. Initialise pi distribution to match the reset vector
    (maphash (lambda (node val) (puthash node val pi-table)) reset-vector)

    ;; 4. Iterative Power Method for PageRank: pi = (1-gamma)*r + gamma * P^T * pi [37]
    (cl-loop repeat iters do
             (let ((next-pi (make-hash-table :test 'equal)))
               ;; Add the (1-gamma)*r_q prior to next state
               (maphash (lambda (node r-val)
                          (puthash node (* (- 1.0 gamma) r-val) next-pi))
                        reset-vector)
               ;; Propagate state over transitions: gamma * P^T * pi
               (maphash
                (lambda (u-node u-val)
                  (let ((transitions (gethash u-node adj-list)))
                    (dolist (trans transitions)
                      (let* ((v-node (car trans))
                             (weight (cdr trans))
                             (current-v (gethash v-node next-pi 0.0)))
                        (puthash v-node (+ current-v (* gamma u-val weight)) next-pi)))))
                pi-table)
               (setq pi-table next-pi)))

    pi-table))

(defalias 'macher-agent-zero-mem-pagerank #'macher-agent-zero-mem-pagerank-fixed-point
  "Default Stationary Personalised PageRank distribution over GRAPH using
fixed-point arithmetic.")

(defconst macher-agent-zero-mem-pr-scale 65536
  "Fixed point scaling factor for PageRank.")

(cl-defstruct (macher-agent-zero-mem-pr-ctx
               (:constructor macher-agent-zero-mem-pr-ctx-create)
               (:type vector))
  "Context for allocation-free vectorised PageRank."
  node-count
  pi-array
  next-array
  reset-array
  transitions
  nodes-array)

(defun macher-agent-zero-mem--extract-nodes (adj-list)
  "Extract unique nodes from ADJ-LIST."
  (let ((nodes nil))
    (maphash (lambda (node _trans) (push node nodes)) adj-list)
    nodes))

(defun macher-agent-zero-mem--map-nodes (nodes)
  "Map NODES to indices and return (node-to-idx-hash . idx-to-node-vector)."
  (let* ((count (length nodes))
         (node-to-idx (make-hash-table :test 'equal :size count))
         (idx-to-node (make-vector count nil))
         (i 0))
    (dolist (n nodes)
      (puthash n i node-to-idx)
      (aset idx-to-node i n)
      (setq i (1+ i)))
    (cons node-to-idx idx-to-node)))

(defun macher-agent-zero-mem--compile-transitions (adj-list node-to-idx count)
  "Compile ADJ-LIST into a vector of transitions using fixed-point weights.
Argument NODE-TO-IDX node index.
Argument COUNT total."
  (let ((transitions (make-vector count nil)))
    (maphash
     (lambda (u-node trans-list)
       (let ((u-idx (gethash u-node node-to-idx))
             (compiled-trans nil))
         (dolist (trans trans-list)
           (let ((v-idx (gethash (car trans) node-to-idx))
                 (w-fp (round (* (cdr trans) macher-agent-zero-mem-pr-scale))))
             (push (cons v-idx w-fp) compiled-trans)))
         (aset transitions u-idx compiled-trans)))
     adj-list)
    transitions))

(defun macher-agent-zero-mem--init-reset-vector (query graph nodes node-to-idx count)
  "Initialise the vectorised reset distribution."
  (let ((reset-array (make-vector count 0))
        (alignments (macher-agent-zero-mem-align-query query graph)))
    (let ((align-sum (cl-loop for (_node . val) in alignments sum val)))
      (if (> align-sum 0.0)
          (dolist (align alignments)
            (let ((idx (gethash (car align) node-to-idx)))
              (when idx
                (aset reset-array idx (round (* macher-agent-zero-mem-pr-scale (/ (cdr align) align-sum)))))))
        (let* ((doc-count 0)
               (doc-nodes nil))
          (dolist (node nodes)
            (when (eq (car node) :doc)
              (push node doc-nodes)
              (setq doc-count (1+ doc-count))))
          (if (> doc-count 0)
              (let ((prob-fp (round (/ (float macher-agent-zero-mem-pr-scale) doc-count))))
                (dolist (dn doc-nodes)
                  (let ((idx (gethash dn node-to-idx)))
                    (when idx
                      (aset reset-array idx prob-fp)))))
            (let ((prob-fp (round (/ (float macher-agent-zero-mem-pr-scale) count))))
              (dolist (node nodes)
                (let ((idx (gethash node node-to-idx)))
                  (when idx
                    (aset reset-array idx prob-fp)))))))))
    reset-array))

(defun macher-agent-zero-mem--pr-tick (ctx gamma-fp one-minus-gamma-fp)
  "Vectorised, allocation-free hot loop iteration for PageRank."
  (let* ((count (macher-agent-zero-mem-pr-ctx-node-count ctx))
         (pi-array (macher-agent-zero-mem-pr-ctx-pi-array ctx))
         (next-array (macher-agent-zero-mem-pr-ctx-next-array ctx))
         (reset-array (macher-agent-zero-mem-pr-ctx-reset-array ctx))
         (transitions (macher-agent-zero-mem-pr-ctx-transitions ctx)))
    ;; Add (1-gamma)*r_q prior to all nodes
    (dotimes (i count)
      (aset next-array i (ash (* one-minus-gamma-fp (aref reset-array i)) -16)))

    ;; Propagate state over transitions: gamma * P^T * pi
    (dotimes (u-idx count)
      (let ((u-val (aref pi-array u-idx)))
        (when (> u-val 0)
          (let ((trans-list (aref transitions u-idx)))
            (while trans-list
              (let* ((trans (car trans-list))
                     (v-idx (car trans))
                     (w-fp (cdr trans))
                     (delta (ash (* u-val w-fp gamma-fp) -32)))
                (aset next-array v-idx (+ (aref next-array v-idx) delta)))
              (setq trans-list (cdr trans-list)))))))))

(defun macher-agent-zero-mem--swap-arrays (ctx)
  "Swap arrays in CTX to avoid allocation."
  (let ((tmp (macher-agent-zero-mem-pr-ctx-pi-array ctx)))
    (setf (macher-agent-zero-mem-pr-ctx-pi-array ctx) (macher-agent-zero-mem-pr-ctx-next-array ctx))
    (setf (macher-agent-zero-mem-pr-ctx-next-array ctx) tmp)))

(defun macher-agent-zero-mem--export-scores (ctx)
  "Convert the vectorised pi-array back to a float-based hash table."
  (let* ((count (macher-agent-zero-mem-pr-ctx-node-count ctx))
         (pi-array (macher-agent-zero-mem-pr-ctx-pi-array ctx))
         (nodes-array (macher-agent-zero-mem-pr-ctx-nodes-array ctx))
         (result-table (make-hash-table :test 'equal :size count))
         (scale-f (float macher-agent-zero-mem-pr-scale)))
    (dotimes (i count)
      (puthash (aref nodes-array i) (/ (float (aref pi-array i)) scale-f) result-table))
    result-table))

(defun macher-agent-zero-mem-pagerank-fixed-point (query graph &optional iterations)
  "Compute Stationary Personalised PageRank using refactored allocation-free
vector-driven fixed-point arithmetic. ITERATIONS defaults to 15. Returns a
hash table mapping nodes to PageRank scores."
  (let* ((iters (or iterations 15))
         (gamma-fp (round (* macher-agent-zero-mem-damping-factor macher-agent-zero-mem-pr-scale)))
         (one-minus-gamma-fp (- macher-agent-zero-mem-pr-scale gamma-fp))
         (adj-list (macher-agent-zero-mem-graph-adj-list graph))
         (nodes (macher-agent-zero-mem--extract-nodes adj-list))
         (count (length nodes)))

    (if (= count 0)
        (make-hash-table :test 'equal)
      (let* ((mapped (macher-agent-zero-mem--map-nodes nodes))
             (node-to-idx (car mapped))
             (idx-to-node (cdr mapped))
             (transitions (macher-agent-zero-mem--compile-transitions adj-list node-to-idx count))
             (reset-array (macher-agent-zero-mem--init-reset-vector query graph nodes node-to-idx count))
             (pi-array (make-vector count 0))
             (next-array (make-vector count 0))
             (ctx (macher-agent-zero-mem-pr-ctx-create
                   :node-count count
                   :pi-array pi-array
                   :next-array next-array
                   :reset-array reset-array
                   :transitions transitions
                   :nodes-array idx-to-node)))

        ;; Initialise pi distribution to match reset vector
        (dotimes (i count)
          (aset pi-array i (aref reset-array i)))

        ;; Iterative Power Method with fixed-point arithmetic & zero-alloc buffer swapping
        (dotimes (_ iters)
          (macher-agent-zero-mem--pr-tick ctx gamma-fp one-minus-gamma-fp)
          (macher-agent-zero-mem--swap-arrays ctx))

        ;; Convert scores back to float
        (macher-agent-zero-mem--export-scores ctx)))))


;;;; 4. Dual-View Evidence Retrieval and Fusion

(cl-defun macher-agent-zero-mem-retrieve (query graph &key (top-k 5) (iterations 15) (algorithm 'fixed-point))
  "Execute the Dual-View Evidence retrieval and closure pipeline [41, 42].
ALGORITHM specifies PageRank implementation: `fixed-point' (default) or `float'.
Returns the top-K highest-ranked `macher-agent-zero-mem-trace' structs."
  (let* ((pi-table (if (eq algorithm 'float)
                       (macher-agent-zero-mem-pagerank-float query graph iterations)
                     (macher-agent-zero-mem-pagerank-fixed-point query graph iterations)))
         (doc-scores nil)
         (traces-ht (macher-agent-zero-mem-graph-traces graph)))

    ;; 1. Filter and normalise document scores [41]
    (maphash
     (lambda (node score)
       (when (eq (car node) :doc)
         (push (cons (cdr node) score) doc-scores)))
     pi-table)

    (setq doc-scores (sort doc-scores (lambda (a b) (> (cdr a) (cdr b)))))

    ;; 2. Retrieve top-K document nodes [55]
    (let ((top-ids (cl-loop for (id . _score) in doc-scores
                            repeat top-k
                            collect id))
          (results nil))
      (dolist (id top-ids)
        (let ((trace (gethash id traces-ht)))
          (when trace
            (push trace results))))
      (nreverse results))))

(provide 'macher-agent-zero-mem)
;;; macher-agent-zero-mem.el ends here
