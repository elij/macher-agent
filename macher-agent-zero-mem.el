;;; macher-agent-zero-mem.el --- Zero-Token Memory Operations for macher-agent -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Autonomous Zero-Token Memory (Zero-Mem) plugin for macher-agent.
;;
;; It features:
;; 1. A Naive deterministic Elisp Named Entity Recognition (NER) extractor.
;; 2. Bipartite Entity-Context Graph construction with chronological trace adjacency.
;; 3. Stationary Personalised PageRank (PPR) diffusion mathematically modeled in Lisp.
;; 4. Chronological/Temporal trace indexing demarcating prompt/response turns and offsets.
;; 5. Non-destructive transmission wire pruning protecting live Emacs buffers.
;; 6. Event horizon query filtering preventing active context duplication.
;; 7. Stationary calculated parent graph snapshots for subagent retrieval.
;; 8. Autonomous plugin lifecycle management via dynamic pipeline and hook registration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'macher-agent-core)
(require 'macher-agent-gptel)
(require 'macher-agent-tools)

(defvar macher-agent--routing-stack nil)

(defgroup macher-agent-zero-mem nil
  "Zero-Token Memory Operations for LLM agents."
  :group 'macher-agent
  :prefix "macher-agent-zero-mem-")

(defcustom macher-agent-zero-mem-damping-factor 0.6
  "The damping factor (gamma) for Personalised PageRank diffusion."
  :type 'float
  :group 'macher-agent-zero-mem)

(defcustom macher-agent-zero-mem-entity-transition-ratio 0.7
  "Ratio of document transitions vs temporal neighbors (E_dd)."
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
  "Extract entity strings from TEXT using deterministic heuristic rules.
Return a deduplicated list of lowercase strings representing the entities."
  (let ((case-fold-search nil) ; Case-sensitive matching
        (entities nil))
    (with-temp-buffer
      (insert text)

      ;; Heuristic 1: Multi-word/Single Proper Nouns
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
  "Represents an individual context unit/document node (V_d)."
  id         ; Integer unique ID
  text       ; Raw string content
  timestamp  ; Float time or integer sequence
  entities   ; List of extracted entity strings
  metadata)  ; Extensible plist (for example, :session, :speaker, :line, :offset, :turn, :type)

(cl-defstruct (macher-agent-zero-mem-graph
               (:constructor macher-agent-zero-mem-graph-create)
               (:type list))
  "Represents the bipartite relational trace graph."
  traces        ; Hash table: ID -> `macher-agent-zero-mem-trace'
  entity-index  ; Hash table: Entity -> List of trace IDs containing it (inverse index)
  adj-list)     ; Hash table: Node -> List of (NeighborNode . EdgeWeight)

(defun macher-agent-zero-mem-build-graph (traces-raw)
  "Build the relational trace graph from a list of plist raw traces TRACES-RAW.
TRACES-RAW should be a list of plists
example (:text \"text\" :timestamp 1.0 :metadata plist).
Return a `macher-agent-zero-mem-graph' struct."
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
             (raw-id (or (plist-get raw :id) id-counter))
             (ents (macher-agent-zero-mem-naive-ner text))
             (trace (macher-agent-zero-mem-trace-create
                     :id raw-id
                     :text text
                     :timestamp ts
                     :entities ents
                     :metadata meta)))
        (puthash raw-id trace traces)
        (push trace trace-list)
        (setq id-counter (1+ id-counter))))

    (setq trace-list (nreverse trace-list))

    ;; Step 2: Build the Entity Reverse Index (V_e)
    (dolist (trace trace-list)
      (let ((tid (macher-agent-zero-mem-trace-id trace)))
        (dolist (ent (macher-agent-zero-mem-trace-entities trace))
          (puthash ent (cons tid (gethash ent entity-index)) entity-index))))

    ;; Step 3: Populate Adjacency Weights (E_de & E_dd)
    (let ((n-traces (length trace-list)))
      (dotimes (i n-traces)
        (let* ((trace (nth i trace-list))
               (tid (macher-agent-zero-mem-trace-id trace))
               (ents (macher-agent-zero-mem-trace-entities trace))
               (e-weight-sum (float (length ents)))
               (doc-node (cons :doc tid))
               (doc-transitions nil))

          ;; Transition from Document to Entities (E_de)
          (when (> e-weight-sum 0.0)
            (dolist (ent ents)
              (let* ((raw-w (/ 1.0 e-weight-sum))
                     (scaled-w (* macher-agent-zero-mem-entity-transition-ratio raw-w)))
                (push (cons (cons :ent ent) scaled-w) doc-transitions))))

          ;; Transition to Chronological Neighbors (E_dd)
          (let ((neighbors nil))
            (when (> i 0)
              (push (macher-agent-zero-mem-trace-id (nth (1- i) trace-list)) neighbors))
            (when (< i (1- n-traces))
              (push (macher-agent-zero-mem-trace-id (nth (1+ i) trace-list)) neighbors))
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
             (push (cons (cons :doc tid) (/ 1.0 n-tids)) ent-transitions))
           (puthash ent-node ent-transitions adj-list)))
       entity-index))

    (macher-agent-zero-mem-graph-create
     :traces traces
     :entity-index entity-index
     :adj-list adj-list)))


;;;; 3. Query Alignment and PageRank Diffusion

(defun macher-agent-zero-mem-align-query (query graph)
  "Extract and align QUERY entities with GRAPH's Entity nodes.
Return an association list of (EntityNode . ActivationScore)."
  (let* ((case-fold-search t)
         (entity-index (macher-agent-zero-mem-graph-entity-index graph))
         (q-str (if (listp query) (string-join query " ") query))
         (q-entities (or (macher-agent-zero-mem-naive-ner q-str)
                         (split-string (downcase q-str) "[^[:alnum:]-_]+" t)))
         (aligned-activations nil)
         (seen-ents (make-hash-table :test 'equal)))
    (dolist (q-ent q-entities)
      (when (and q-ent (> (length q-ent) 1))
        (maphash
         (lambda (g-ent _tids)
           (when g-ent
             (cond
              ((and (string-equal-ignore-case q-ent g-ent)
                    (not (gethash g-ent seen-ents)))
               (puthash g-ent t seen-ents)
               (push (cons (cons :ent g-ent) 1.0) aligned-activations))
              ((and (or (string-match-p (regexp-quote q-ent) g-ent)
                        (string-match-p (regexp-quote g-ent) q-ent))
                    (not (gethash g-ent seen-ents)))
               (puthash g-ent t seen-ents)
               (push (cons (cons :ent g-ent) 0.8) aligned-activations)))))
         entity-index)))
    aligned-activations))

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

ADJ-LIST is the adjacency list hash table.
NODE-TO-IDX is the node-to-index mapping hash table.
COUNT is the total node count."
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
  "Initialise vectorised reset distribution for QUERY on GRAPH.
Use NODES, NODE-TO-IDX, and COUNT."
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
  "Vectorised, allocation-free hot loop iteration for PageRank.
Use CTX, GAMMA-FP, and ONE-MINUS-GAMMA-FP."
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
  "Convert the vectorised pi-array in CTX back to a float-based hash table."
  (let* ((count (macher-agent-zero-mem-pr-ctx-node-count ctx))
         (pi-array (macher-agent-zero-mem-pr-ctx-pi-array ctx))
         (nodes-array (macher-agent-zero-mem-pr-ctx-nodes-array ctx))
         (result-table (make-hash-table :test 'equal :size count))
         (scale-f (float macher-agent-zero-mem-pr-scale)))
    (dotimes (i count)
      (puthash (aref nodes-array i) (/ (float (aref pi-array i)) scale-f) result-table))
    result-table))

(defun macher-agent-zero-mem-pagerank-fixed-point (query graph &optional iterations)
  "Compute Stationary Personalised PageRank for QUERY on GRAPH.
Use fixed-point arithmetic.  ITERATIONS defaults to 15.  Return a hash table
mapping nodes to PageRank scores."
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

        ;; Iterative Power Method with fixed-point arithmetic and zero-alloc buffer swapping
        (dotimes (_ iters)
          (macher-agent-zero-mem--pr-tick ctx gamma-fp one-minus-gamma-fp)
          (macher-agent-zero-mem--swap-arrays ctx))

        ;; Convert scores back to float
        (macher-agent-zero-mem--export-scores ctx)))))


;;;; 4. Dual-View Evidence Retrieval and Fusion

(cl-defun macher-agent-zero-mem-retrieve (query graph &key (top-k 5) (iterations 15) (algorithm 'fixed-point))
  "Execute Dual-View Evidence retrieval for QUERY on GRAPH.
ITERATIONS specifies the number of PageRank diffusion iterations.
ALGORITHM can be `fixed-point' (default) or `float'.
Return the TOP-K highest-ranked `macher-agent-zero-mem-trace' structs."
  (when (and graph (macher-agent-zero-mem-align-query query graph))
    (let* ((pi-table (if (and (eq algorithm 'float)
                              (fboundp 'macher-agent-zero-mem-pagerank-float))
                         (funcall 'macher-agent-zero-mem-pagerank-float query graph iterations)
                       (macher-agent-zero-mem-pagerank-fixed-point query graph iterations)))
           (doc-scores nil)
           (traces-ht (macher-agent-zero-mem-graph-traces graph)))

      ;; 1. Filter and normalise document scores
      (maphash
       (lambda (node score)
         (when (eq (car node) :doc)
           (push (cons (cdr node) score) doc-scores)))
       pi-table)

      (setq doc-scores (sort doc-scores (lambda (a b) (> (cdr a) (cdr b)))))

      ;; 2. Retrieve top-K document nodes
      (let ((top-ids (cl-loop for (id . _score) in doc-scores
                              repeat top-k
                              collect id))
            (results nil))
        (dolist (id top-ids)
          (let ((trace (gethash id traces-ht)))
            (when trace
              (push trace results))))
        (nreverse results)))))


;;;; 5. Event Horizon and Trace Indexing

(defvar-local macher-agent-zero-mem--event-horizon nil
  "Buffer-local event horizon plist (:line L :offset O) demarcating truncation boundary.")

(defun macher-agent-zero-mem--calculate-event-horizon (orig-buf)
  "Calculate the event horizon (:line LINE :offset OFFSET) for ORIG-BUF based on context limits."
  (when (and orig-buf (buffer-live-p orig-buf))
    (with-current-buffer orig-buf
      (let ((max-chars (macher-agent--get-max-context-chars orig-buf)))
        (if (<= (buffer-size) max-chars)
            (list :line 1 :offset (point-min))
          (save-excursion
            (goto-char (point-min))
            (let* ((safe-min (if (and (looking-at "^---\n")
                                      (re-search-forward "^---\n" nil t 2))
                                 (point)
                               (point-min)))
                   (target-pt (max safe-min (- (point-max) max-chars)))
                   (safe-pt safe-min)
                   (found nil)
                   (match nil)
                   (safe-boundary-p
                    (lambda (m)
                      (let* ((end-pt (prop-match-end m))
                             (next-pt (save-excursion
                                        (goto-char end-pt)
                                        (if-let* ((next-m (text-property-search-forward 'gptel 'response t)))
                                            (prop-match-beginning next-m)
                                          (point-max)))))
                        (not (save-excursion
                               (goto-char end-pt)
                               (re-search-forward "^[ \t]*\\(```\\|#\\+begin_src \\)tool" next-pt t)))))))
              (goto-char target-pt)
              (while (and (not found)
                          (setq match (text-property-search-backward 'gptel 'response t))
                          (>= (prop-match-beginning match) safe-min))
                (when (funcall safe-boundary-p match)
                  (setq safe-pt (prop-match-end match) found t)))
              (unless found
                (goto-char target-pt)
                (while (and (not found)
                            (setq match (text-property-search-forward 'gptel 'response t)))
                  (when (funcall safe-boundary-p match)
                    (setq safe-pt (prop-match-end match) found t))))
              (list :line (line-number-at-pos safe-pt)
                    :offset safe-pt))))))))

(defun macher-agent-zero-mem--get-event-horizon (orig-buf &optional context)
  "Get the event horizon plist for ORIG-BUF or CONTEXT."
  (or (when (and orig-buf (buffer-live-p orig-buf))
        (buffer-local-value 'macher-agent-zero-mem--event-horizon orig-buf))
      (when (and context (macher-agent-context-p context))
        (plist-get (macher-agent-context-plugins context) :event-horizon))
      (when (and orig-buf (buffer-live-p orig-buf))
        (macher-agent-zero-mem--calculate-event-horizon orig-buf))))

(defun macher-agent-zero-mem--buffer-to-traces (buffer &optional before-offset before-line)
  "Convert BUFFER content into trace plists for Zero-Mem graph construction.
Demarcate completed turns based on prompt/response boundaries and record line numbers and offsets.
If BEFORE-OFFSET or BEFORE-LINE is provided, only include traces strictly before that boundary."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((traces nil)
              (line-num 1)
              (turn-idx 0)
              (last-response :init))
          (while (not (eobp))
            (let* ((line-start (line-beginning-position))
                   (line-end (line-end-position))
                   (line-text (buffer-substring-no-properties line-start line-end))
                   (is-resp (eq (get-text-property line-start 'gptel) 'response)))
              (unless (eq is-resp last-response)
                (setq turn-idx (1+ turn-idx))
                (setq last-response is-resp))
              (unless (string-empty-p (string-trim line-text))
                (let ((within-horizon (and (or (null before-offset) (< line-start before-offset))
                                           (or (null before-line) (< line-num before-line)))))
                  (when within-horizon
                    (push (list :id line-num
                                :text line-text
                                :timestamp (float line-num)
                                :metadata (list :buffer (buffer-name buffer)
                                                :line line-num
                                                :offset line-start
                                                :turn turn-idx
                                                :type (if is-resp :response :prompt)))
                          traces)))))
            (setq line-num (1+ line-num))
            (forward-line 1))
          (nreverse traces))))))

;;;; 5.1 Plugin State Accessors

(defun macher-agent-zero-mem-get-state (context)
  "Retrieve the zero-mem plugin state from CONTEXT.
Returns the value stored under the `:zero-mem' key in `(macher-agent-context-plugins CONTEXT)'."
  (when (macher-agent-context-p context)
    (plist-get (macher-agent-context-plugins context) :zero-mem)))

(defun macher-agent-zero-mem-set-state (context state)
  "Store the zero-mem plugin STATE under `:zero-mem' in CONTEXT.
Returns STATE."
  (when (macher-agent-context-p context)
    (setf (macher-agent-context-plugins context)
          (plist-put (macher-agent-context-plugins context) :zero-mem state)))
  state)

(defvar macher-agent-memory-vector-storage (make-hash-table :test 'equal)
  "Vector storage repository committing interaction histories and trace graphs.

Maps session or buffer identifiers to compiled relational trace graphs.")

(defun macher-agent-memory--persist-interaction (&optional buffer)
  "Commit conversation history in BUFFER to vector storage.

BUFFER is the optional interaction buffer, defaulting to the current buffer.
Demarcates completed turns based on prompt/response boundaries and records line numbers/offsets.

Return the committed vector storage graph structure, or nil if no traces exist.
Side effects: Populates `macher-agent-memory-vector-storage` with interaction traces."
  (let* ((buf (or buffer (current-buffer)))
         (buf-name (cond
                    ((bufferp buf) (buffer-name buf))
                    ((stringp buf) buf)
                    (t (format "%s" buf))))
         (target-buf (if (bufferp buf) buf (get-buffer buf-name))))
    (when (and target-buf (buffer-live-p target-buf))
      (let* ((traces (macher-agent-zero-mem--buffer-to-traces target-buf))
             (graph (when traces (macher-agent-zero-mem-build-graph traces)))
             (context (when (or (local-variable-p 'macher-agent--persistent-context target-buf)
                                (boundp 'macher-agent--persistent-context))
                        (buffer-local-value 'macher-agent--persistent-context target-buf))))
        (when graph
          (puthash buf-name graph macher-agent-memory-vector-storage)
          (when context
            (macher-agent-zero-mem-set-state context graph))
          graph)))))


;;;; 6. Buffer and Context Resolution Helpers

(defun macher-agent-zero-mem--extract-clean-prompt (orig-buf context)
  "Extract the sanitized prompt from CONTEXT or ORIG-BUF.
Uses `macher-agent-context-prompt' and strips local variables and header prefixes."
  (let* ((ctx (or context (when (and orig-buf (buffer-live-p orig-buf))
                            (buffer-local-value 'macher-agent--persistent-context orig-buf))))
         (raw-prompt (or (when (and ctx (macher-agent-context-p ctx))
                           (macher-agent-context-prompt ctx))
                         (when (and orig-buf (buffer-live-p orig-buf))
                           (with-current-buffer orig-buf
                             (buffer-substring-no-properties (point-min) (point-max)))))))
    (when (and raw-prompt (stringp raw-prompt))
      (let ((stripped (replace-regexp-in-string "<!--[[:space:]\n]*Local Variables:[^>]*-->" "" raw-prompt)))
        (string-trim (replace-regexp-in-string "^[#>*[:space:]]+" "" (string-trim stripped)))))))

(defun macher-agent-zero-mem--resolve-parent-buffer (&optional arg1 arg2 &rest _rest)
  "Resolve the parent buffer from CONTEXT or TARGET-BUF or routing stack."
  (let* ((context (cond ((macher-agent-context-p arg1) arg1)
                        ((macher-agent-context-p arg2) arg2)
                        (t nil)))
         (target-buf (cond ((bufferp arg1) arg1)
                           ((bufferp arg2) arg2)
                           ((stringp arg1) (get-buffer arg1))
                           ((stringp arg2) (get-buffer arg2))
                           (t nil))))
    (or (when context
          (let ((orig (or (macher-agent-context-origin-buffer context)
                          (plist-get (macher-agent-context-plugins context) :origin-buffer)
                          (plist-get (macher-agent-context-plugins context) :originator-buffer))))
            (cond ((bufferp orig) (when (buffer-live-p orig) orig))
                  ((stringp orig) (get-buffer orig))
                  (t nil))))
        (let ((buf (or target-buf (current-buffer))))
          (when (and buf (bufferp buf) (buffer-live-p buf))
            (or (when (or (local-variable-p 'macher-agent--persistent-context buf)
                          (boundp 'macher-agent--persistent-context))
                  (let ((ctx (buffer-local-value 'macher-agent--persistent-context buf)))
                    (when (macher-agent-context-p ctx)
                      (let ((orig (or (macher-agent-context-origin-buffer ctx)
                                      (plist-get (macher-agent-context-plugins ctx) :origin-buffer)
                                      (plist-get (macher-agent-context-plugins ctx) :originator-buffer))))
                        (cond ((bufferp orig) (when (buffer-live-p orig) orig))
                              ((stringp orig) (get-buffer orig))
                              (t nil))))))
                (with-current-buffer buf
                  (let ((stack (bound-and-true-p macher-agent--routing-stack)))
                    (when (and stack (listp stack))
                      (let ((top (car stack)))
                        (cond
                         ((bufferp top) (when (buffer-live-p top) top))
                         ((stringp top) (get-buffer top))
                         ((plistp top)
                          (let ((frame-buf (or (plist-get top :originator-buffer)
                                               (plist-get top :origin-buffer)
                                               (plist-get top :parent-buffer)
                                               (plist-get top :originator-name))))
                            (cond ((bufferp frame-buf) (when (buffer-live-p frame-buf) frame-buf))
                                  ((stringp frame-buf) (get-buffer frame-buf))
                                  (t nil))))
                         (t nil))))))))))))


;;;; 7. Transmission Pipeline Steps

(defun macher-agent-memory-pipe--inject-tool (state orig-buf _presets _skills _redirect)
  "Inject the search tool into STATE if buffer exceeds size limits."
  (let* ((buf (or orig-buf
                  (when (macher-agent-transmission-state-p state)
                    (macher-agent-transmission-state-target-buffer state))
                  (current-buffer)))
         (max-chars (macher-agent--get-max-context-chars buf))
         (buf-size (if (and buf (buffer-live-p buf))
                       (with-current-buffer buf (buffer-size))
                     0)))
    (when (> buf-size max-chars)
      (let* ((tools (macher-agent-transmission-state-tools state))
             (mem-tool (or (ignore-errors (macher-agent-resolve-tool 'search_conversation_history nil nil nil))
                           'search_conversation_history))
             (already-has-mem (cl-some (lambda (tl)
                                         (equal (macher-agent-canonical-tool-name tl)
                                                "search_conversation_history"))
                                       tools)))
        (unless already-has-mem
          (setf (macher-agent-transmission-state-tools state)
                (append tools (list mem-tool)))))))
  state)

(defun macher-agent-parent-memory-pipe--inject-tool (state &optional orig-buf _presets _skills _redirect)
  "Inject `search_parent_conversation_history' into STATE if a live parent buffer is detected."
  (let* ((ctx (when (macher-agent-transmission-state-p state)
                (macher-agent-transmission-state-context state)))
         (target-buf (or (when (macher-agent-transmission-state-p state)
                           (macher-agent-transmission-state-target-buffer state))
                         orig-buf
                         (current-buffer)))
         (parent (macher-agent-zero-mem--resolve-parent-buffer ctx target-buf)))
    (when (and parent (buffer-live-p parent))
      (let* ((tools (macher-agent-transmission-state-tools state))
             (parent-tool (or (ignore-errors (macher-agent-resolve-tool 'search_parent_conversation_history nil nil nil))
                              (when (boundp 'macher-agent-search-parent-conversation-history-tool)
                                (symbol-value 'macher-agent-search-parent-conversation-history-tool))
                              'search_parent_conversation_history))
             (already-has-tool (cl-some (lambda (tl)
                                          (equal (macher-agent-canonical-tool-name tl)
                                                 "search_parent_conversation_history"))
                                        tools)))
        (unless already-has-tool
          (setf (macher-agent-transmission-state-tools state)
                (append tools (list parent-tool)))))))
  state)

(defun macher-agent-memory-pipe--truncate-buffer (state orig-buf _presets _skills _redirect)
  "Truncate the transmission buffer non-destructively without mutating ORIG-BUF.
Pruning operates purely on the transmission buffer / ephemeral wire payload."
  (let* ((tx-buf (current-buffer))
         (ref-buf (if (and orig-buf (buffer-live-p orig-buf)) orig-buf tx-buf))
         (max-chars (macher-agent--get-max-context-chars ref-buf)))
    (when (> (buffer-size tx-buf) max-chars)
      (save-excursion
        (goto-char (point-min))
        (let* ((safe-min (if (and (looking-at "^---\n")
                                  (re-search-forward "^---\n" nil t 2))
                             (point)
                           (point-min)))
               (target-pt (max safe-min (- (point-max) max-chars)))
               (safe-pt safe-min)
               (found nil)
               (match nil)
               (safe-boundary-p
                (lambda (m)
                  (let* ((end-pt (prop-match-end m))
                         (next-pt (save-excursion
                                    (goto-char end-pt)
                                    (if-let* ((next-m (text-property-search-forward 'gptel 'response t)))
                                        (prop-match-beginning next-m)
                                      (point-max)))))
                    (not (save-excursion
                           (goto-char end-pt)
                           (re-search-forward "^[ \t]*\\(```\\|#\\+begin_src \\)tool" next-pt t)))))))
          (goto-char target-pt)
          (while (and (not found)
                      (setq match (text-property-search-backward 'gptel 'response t))
                      (>= (prop-match-beginning match) safe-min))
            (when (funcall safe-boundary-p match)
              (setq safe-pt (prop-match-end match) found t)))
          (unless found
            (goto-char target-pt)
            (while (and (not found)
                        (setq match (text-property-search-forward 'gptel 'response t)))
              (when (funcall safe-boundary-p match)
                (setq safe-pt (prop-match-end match) found t))))
          (when (> safe-pt safe-min)
            (let ((lines-deleted (count-lines safe-min safe-pt))
                  (horizon-line (line-number-at-pos safe-pt)))
              ;; Record event horizon on orig-buf without modifying its text content
              (when (and orig-buf (buffer-live-p orig-buf))
                (with-current-buffer orig-buf
                  (setq-local macher-agent-zero-mem--event-horizon
                              (list :line horizon-line :offset safe-pt)))
                (when (boundp 'macher-agent-memory-vector-storage)
                  (remhash (buffer-name orig-buf) macher-agent-memory-vector-storage)))
              ;; Truncate the transmission wire buffer (current-buffer)
              (delete-region safe-min safe-pt)
              (goto-char safe-min)
              (insert (format "\n[... SYSTEM ALERT: macher-agent truncated %d lines of early history to conserve tokens. If you need this context, use the `search_conversation_history` tool ...]\n\n" lines-deleted))
              (when (looking-at "^[ \t\n\r]+")
                (replace-match ""))))))))
  state)

(defun macher-agent-pipe--inject-zero-mem (state orig-buf _presets _skills _redirect)
  "Retrieve relevant historical traces and inject them into STATE's directives."
  (let* ((buf (or orig-buf
                  (when (macher-agent-transmission-state-p state)
                    (macher-agent-transmission-state-target-buffer state))
                  (current-buffer)))
         (context (when (and buf (buffer-live-p buf)
                             (or (local-variable-p 'macher-agent--persistent-context buf)
                                 (boundp 'macher-agent--persistent-context)))
                    (buffer-local-value 'macher-agent--persistent-context buf)))
         (graph (or (when context
                      (macher-agent-zero-mem-get-state context))
                    (when (boundp 'macher-agent-memory-vector-storage)
                      (gethash (buffer-name buf) macher-agent-memory-vector-storage))))
         (query (macher-agent-zero-mem--extract-clean-prompt buf context)))
    (when (and graph query (not (string-empty-p query)))
      (let* ((traces (macher-agent-zero-mem-retrieve query graph :top-k 5))
             (formatted-traces nil))
        (dolist (tr traces)
          (let ((line (or (plist-get (macher-agent-zero-mem-trace-metadata tr) :line)
                          (macher-agent-zero-mem-trace-id tr)
                          (plist-get tr :id)))
                (text (macher-agent-zero-mem-trace-text tr)))
            (push (format "<trace id=\"%s\">\n%s\n</trace>" line text) formatted-traces)))
        (when formatted-traces
          (let ((evidence-block
                 (format "<historical_context>\nCRITICAL: The following historical traces were retrieved via Zero-Mem relative to the user prompt:\n%s\n</historical_context>"
                         (string-join (nreverse formatted-traces) "\n"))))
            (setf (macher-agent-transmission-state-directives state)
                  (append (macher-agent-transmission-state-directives state)
                          (list evidence-block)))))))
    state))

(defun macher-agent-pipe--inject-parent-context (state &optional orig-buf _presets _skills _redirect)
  "Retrieve relevant historical traces from stationary parent graph snapshot and inject into STATE."
  (let* ((ctx (when (macher-agent-transmission-state-p state)
                (macher-agent-transmission-state-context state)))
         (target-buf (or (when (macher-agent-transmission-state-p state)
                           (macher-agent-transmission-state-target-buffer state))
                         orig-buf
                         (current-buffer)))
         (parent (macher-agent-zero-mem--resolve-parent-buffer ctx target-buf)))
    (when (and parent (buffer-live-p parent))
      (let* ((parent-ctx (when (or (local-variable-p 'macher-agent--persistent-context parent)
                                   (boundp 'macher-agent--persistent-context))
                           (buffer-local-value 'macher-agent--persistent-context parent)))
             ;; Subagents use stationary calculated parent graph snapshot at delegation time without re-indexing
             (parent-graph (or (when (boundp 'macher-agent-memory-vector-storage)
                                 (gethash (buffer-name parent) macher-agent-memory-vector-storage))
                               (when parent-ctx
                                 (macher-agent-zero-mem-get-state parent-ctx))
                               (let ((raw-traces (macher-agent-zero-mem--buffer-to-traces parent)))
                                 (when raw-traces
                                   (let ((g (macher-agent-zero-mem-build-graph raw-traces)))
                                     (when (boundp 'macher-agent-memory-vector-storage)
                                       (puthash (buffer-name parent) g macher-agent-memory-vector-storage))
                                     (when parent-ctx
                                       (macher-agent-zero-mem-set-state parent-ctx g))
                                     g)))))
             (query (macher-agent-zero-mem--extract-clean-prompt target-buf ctx)))
        (when (and parent-graph query (not (string-empty-p query)))
          (let* ((traces (macher-agent-zero-mem-retrieve query parent-graph :top-k 5))
                 (formatted-traces nil))
            (dolist (tr traces)
              (let ((line (or (plist-get (macher-agent-zero-mem-trace-metadata tr) :line)
                              (macher-agent-zero-mem-trace-id tr)
                              (plist-get tr :id)))
                    (text (macher-agent-zero-mem-trace-text tr)))
                (push (format "<trace id=\"%s\">\n%s\n</trace>" line text) formatted-traces)))
            (when formatted-traces
              (let* ((traces-content (string-join (nreverse formatted-traces) "\n"))
                     (evidence-block (format "<parent_conversation_context>\n%s\n</parent_conversation_context>" traces-content)))
                (setf (macher-agent-transmission-state-directives state)
                      (append (macher-agent-transmission-state-directives state)
                              (list evidence-block)))))))))
    state))

(defun macher-agent-memory-pipe--inject-directive (state _orig-buf _presets _skills _redirect)
  "Append search directive to STATE if memory tools are active."
  (let* ((tools (macher-agent-transmission-state-tools state))
         (has-mem (cl-some (lambda (tl)
                             (equal (macher-agent-canonical-tool-name tl)
                                    "search_conversation_history"))
                           tools)))
    (when has-mem
      (let ((directive "CRITICAL DIRECTIVE: Early conversation history has been truncated. You MUST use the `search_conversation_history` tool to retrieve missing context if necessary."))
        (push directive (macher-agent-transmission-state-directives state)))))
  state)

(defun macher-agent-parent-memory-pipe--inject-directive (state _orig-buf _presets _skills _redirect)
  "Append parent search directive to STATE if parent search tool is active."
  (let* ((tools (macher-agent-transmission-state-tools state))
         (has-parent-tool (cl-some (lambda (tl)
                                     (equal (macher-agent-canonical-tool-name tl)
                                            "search_parent_conversation_history"))
                                   tools)))
    (when has-parent-tool
      (let ((directive "CRITICAL DIRECTIVE: You are a specialised child sub-agent spawned to execute a specific, narrow task. Your sole objective is to complete your assigned prompt. You have read-only access to the parent orchestrator's memory via the `search_parent_conversation_history` tool. Treat the parent's history strictly as a reference archive to extract missing variables, definitions, or background facts required for your specific sub-task. Leave all orchestration, task delegation, and overarching goals to the parent."))
        (push directive (macher-agent-transmission-state-directives state)))))
  state)


;;;; 8. Search Backend with Event Horizon Filtering

(defun macher-agent-memory-search-zero-mem (keywords orig-buf &optional ctx-lines)
  "Search for KEYWORDS in ORIG-BUF using zero-mem PageRank search.
For ORIG-BUF that has been truncated, only return document traces located before
the event horizon boundary so the model does not duplicate active context."
  (if (not (and orig-buf (buffer-live-p orig-buf)))
      "Error: Cannot locate original conversation buffer."
    (let* ((buf-name (buffer-name orig-buf))
           (top-k (if (and ctx-lines (> ctx-lines 0)) ctx-lines 5))
           (kws (if (listp keywords) keywords (list keywords)))
           (context (when (and orig-buf (buffer-live-p orig-buf)
                               (or (local-variable-p 'macher-agent--persistent-context orig-buf)
                                   (boundp 'macher-agent--persistent-context)))
                      (buffer-local-value 'macher-agent--persistent-context orig-buf)))
           (horizon (macher-agent-zero-mem--get-event-horizon orig-buf context))
           (horizon-line (plist-get horizon :line))
           (horizon-offset (plist-get horizon :offset))
           (is-truncated (and horizon-offset (> horizon-offset (with-current-buffer orig-buf (point-min)))))
           ;; Subagents/queries use stationary calculated graph snapshot if available
           (graph (or (unless is-truncated
                        (or (when (boundp 'macher-agent-memory-vector-storage)
                              (gethash buf-name macher-agent-memory-vector-storage))
                            (when context
                              (macher-agent-zero-mem-get-state context))))
                      (when (and is-truncated (boundp 'macher-agent-memory-vector-storage))
                        (gethash buf-name macher-agent-memory-vector-storage))
                      (let ((raw-traces (if is-truncated
                                            (macher-agent-zero-mem--buffer-to-traces orig-buf horizon-offset horizon-line)
                                          (macher-agent-zero-mem--buffer-to-traces orig-buf))))
                        (when raw-traces
                          (let ((g (macher-agent-zero-mem-build-graph raw-traces)))
                            (when (boundp 'macher-agent-memory-vector-storage)
                              (puthash buf-name g macher-agent-memory-vector-storage))
                            (when (and context (not is-truncated))
                              (macher-agent-zero-mem-set-state context g))
                            g))))))
      (if (null graph)
          (format "No matches found in history for: %s" (string-join kws ", "))
        (let* ((retrieved (macher-agent-zero-mem-retrieve keywords graph :top-k (if is-truncated (max (* top-k 4) 20) top-k)))
               (results nil))
          (dolist (tr retrieved)
            (let* ((meta (macher-agent-zero-mem-trace-metadata tr))
                   (line (or (plist-get meta :line)
                             (macher-agent-zero-mem-trace-id tr)
                             (plist-get tr :id)))
                   (offset (plist-get meta :offset))
                   (text (macher-agent-zero-mem-trace-text tr))
                   ;; Event horizon query filtering
                   (valid (if is-truncated
                              (and (or (null horizon-line) (< line horizon-line))
                                   (or (null horizon-offset) (null offset) (< offset horizon-offset)))
                            t)))
              (when (and valid (< (length results) top-k))
                (push (format "--- Match near line %d ---\n%s\n" line text) results))))
          (if results
              (string-join (nreverse results) "\n")
            (format "No matches found in history for: %s" (string-join kws ", "))))))))


;;;; 9. Plugin Lifecycle Management

(defun macher-agent-zero-mem-install ()
  "Install zero-token memory hooks and dynamic pipeline steps."
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-memory-pipe--inject-tool 45)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-parent-memory-pipe--inject-tool 46)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-memory-pipe--truncate-buffer 55)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-zero-mem 75)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-parent-context 76)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-memory-pipe--inject-directive 85)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-parent-memory-pipe--inject-directive 86)
  (add-hook 'macher-agent-task-flush-hook #'macher-agent-memory--persist-interaction)
  (setq macher-agent-search-backend-function #'macher-agent-memory-search-zero-mem))

(defun macher-agent-zero-mem-uninstall ()
  "Uninstall zero-token memory hooks and dynamic pipeline steps."
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-memory-pipe--inject-tool)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-parent-memory-pipe--inject-tool)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-memory-pipe--truncate-buffer)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-pipe--inject-zero-mem)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-pipe--inject-parent-context)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-memory-pipe--inject-directive)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-parent-memory-pipe--inject-directive)
  (remove-hook 'macher-agent-task-flush-hook #'macher-agent-memory--persist-interaction)
  (setq macher-agent-search-backend-function #'macher-agent-search-glob))

(provide 'macher-agent-zero-mem)
;;; macher-agent-zero-mem.el ends here
