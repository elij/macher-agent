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
(require 'macher-agent-core)
(require 'macher-agent-gptel)
(require 'macher-agent-tools)

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
  "Represents an individual context unit/document node (V_d)."
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

    ;; Step 3: Populate Adjacency Weights (E_de & E_dd)
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
              ;; w(d_i, e) is c(e, d_i)/sum(c(e', d_i)).
              ;; For naive NER, occurrences are binary, so weight is 1.0 / count(entities)
              (let* ((raw-w (/ 1.0 e-weight-sum))
                     (scaled-w (* macher-agent-zero-mem-entity-transition-ratio raw-w)))
                (push (cons (cons :ent ent) scaled-w) doc-transitions))))

          ;; Transition to Chronological Neighbors (E_dd)
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
             ;; Uniform transition back to documents that contain the entity
             (push (cons (cons :doc tid) (/ 1.0 n-tids)) ent-transitions))
           (puthash ent-node ent-transitions adj-list)))
       entity-index))

    (macher-agent-zero-mem-graph-create
     :traces traces
     :entity-index entity-index
     :adj-list adj-list)))


;;;; 3. Query Alignment & PageRank Diffusion

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

        ;; Iterative Power Method with fixed-point arithmetic & zero-alloc buffer swapping
        (dotimes (_ iters)
          (macher-agent-zero-mem--pr-tick ctx gamma-fp one-minus-gamma-fp)
          (macher-agent-zero-mem--swap-arrays ctx))

        ;; Convert scores back to float
        (macher-agent-zero-mem--export-scores ctx)))))


;;;; 4. Dual-View Evidence Retrieval and Fusion

(cl-defun macher-agent-zero-mem-retrieve (query graph &key (top-k 5) (iterations 15))
  "Execute Dual-View Evidence retrieval for QUERY on GRAPH.
ITERATIONS specifies the number of PageRank diffusion iterations.
Return the TOP-K highest-ranked `macher-agent-zero-mem-trace' structs."
  (when (and graph (macher-agent-zero-mem-align-query query graph))
    (let* ((pi-table (macher-agent-zero-mem-pagerank-fixed-point query graph iterations))
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

(defvar macher-agent-memory-vector-storage (make-hash-table :test 'equal)
  "Vector storage repository committing interaction histories and trace graphs.

Maps session or buffer identifiers to compiled relational trace graphs.")

(defun macher-agent-memory--persist-interaction (&optional buffer)
  "Commit conversation history in BUFFER to vector storage.

BUFFER is the optional interaction buffer, defaulting to the current buffer.

Return the committed vector storage graph structure, or nil if no traces exist.

Side effects: Populates `macher-agent-memory-vector-storage` with interaction traces."
  (let* ((buf (or buffer (current-buffer)))
         (buf-name (cond
                    ((bufferp buf) (buffer-name buf))
                    ((stringp buf) buf)
                    (t (format "%s" buf))))
         (target-buf (if (bufferp buf) buf (get-buffer buf-name))))
    (when (and target-buf (buffer-live-p target-buf))
      (let* ((traces (with-current-buffer target-buf
                       (let ((content (buffer-substring-no-properties (point-min) (point-max)))
                             (lines nil)
                             (idx 0))
                         (dolist (line (split-string content "\n" t))
                           (let ((trimmed (string-trim line)))
                             (unless (string-empty-p trimmed)
                               (setq idx (1+ idx))
                               (push (list :id idx
                                           :text trimmed
                                           :timestamp (float-time)
                                           :metadata (list :buffer buf-name :line idx))
                                     lines))))
                         (nreverse lines))))
             (graph (when traces (macher-agent-zero-mem-build-graph traces))))
        (when graph
          (puthash buf-name graph macher-agent-memory-vector-storage)
          graph)))))

(defun macher-agent-zero-mem--extract-clean-prompt (orig-buf context)
  "Extract the sanitized prompt from CONTEXT or ORIG-BUF.
Uses `macher-agent--get-context-prompt' and strips local variables and header prefixes."
  (let* ((ctx (or context (when (fboundp 'macher-agent-resolve-context)
                            (macher-agent-resolve-context orig-buf))))
         (raw-prompt (when (fboundp 'macher-agent--get-context-prompt)
                       (macher-agent--get-context-prompt ctx))))
    (when (and raw-prompt (stringp raw-prompt))
      (let ((stripped (replace-regexp-in-string "<!--[[:space:]\n]*Local Variables:[^>]*-->" "" raw-prompt)))
        (string-trim (replace-regexp-in-string "^[#>*[:space:]]+" "" (string-trim stripped)))))))

(defun macher-agent-pipe--inject-zero-mem (state orig-buf _presets _skills _redirect)
  "Retrieve relevant historical traces and inject them into STATE's directives."
  (let* ((buf (or orig-buf
                  (when (macher-agent-transmission-state-p state)
                    (macher-agent-transmission-state-target-buffer state))
                  (current-buffer)))
         (context (when (fboundp 'macher-agent-resolve-context)
                    (macher-agent-resolve-context buf)))
         (graph (or (when context
                      (macher-agent--get-context-data context :zero-mem-graph))
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
             (mem-tool (or (ignore-errors (macher-agent-resolve-tool "search_conversation_history" nil nil nil))
                           'search_conversation_history))
             (already-has-mem (cl-some (lambda (tl)
                                         (let ((name (cond
                                                      ((symbolp tl) (symbol-name tl))
                                                      ((stringp tl) tl)
                                                      ((and (fboundp 'gptel-tool-p)
                                                            (gptel-tool-p tl)
                                                            (fboundp 'gptel-tool-name))
                                                       (gptel-tool-name tl))
                                                      ((consp tl) (plist-get tl :name))
                                                      (t nil))))
                                           (and name (string= name "search_conversation_history"))))
                                       tools)))
        (unless already-has-mem
          (setf (macher-agent-transmission-state-tools state)
                (append tools (list mem-tool)))))))
  state)

(defun macher-agent-memory-pipe--truncate-buffer (state orig-buf _presets _skills _redirect)
  "Truncate the physical buffer if it exceeds calculated token limits."
  (when (and orig-buf (buffer-live-p orig-buf))
    (with-current-buffer orig-buf
      (let ((max-chars (macher-agent--get-max-context-chars orig-buf)))
        (when (> (buffer-size) max-chars)
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
                (let ((lines-deleted (count-lines safe-min safe-pt)))
                  (delete-region safe-min safe-pt)
                  (goto-char safe-min)
                  (insert (format "\n[... SYSTEM ALERT: macher-agent truncated %d lines of early history to conserve tokens. If you need this context, use the `search_conversation_history` tool ...]\n\n" lines-deleted))
                  (when (looking-at "^[ \t\n\r]+")
                    (replace-match ""))))))))))
  state)

(defun macher-agent-memory-pipe--inject-directive (state _orig-buf _presets _skills _redirect)
  "Append search directive to STATE if memory tools are active."
  (let* ((tools (macher-agent-transmission-state-tools state))
         (has-mem (cl-some (lambda (tl)
                             (let ((name (cond
                                          ((symbolp tl) (symbol-name tl))
                                          ((stringp tl) tl)
                                          ((and (fboundp 'gptel-tool-p)
                                                (gptel-tool-p tl)
                                                (fboundp 'gptel-tool-name))
                                           (gptel-tool-name tl))
                                          ((consp tl) (plist-get tl :name))
                                          (t nil))))
                               (and name (string= name "search_conversation_history"))))
                           tools)))
    (when has-mem
      (let ((directive "CRITICAL DIRECTIVE: Early conversation history has been truncated. You MUST use the `search_conversation_history` tool to retrieve missing context if necessary."))
        (push directive (macher-agent-transmission-state-directives state)))))
  state)

(defun macher-agent-memory-search-zero-mem (keywords orig-buf &optional ctx-lines)
  "Search for KEYWORDS in ORIG-BUF using zero-mem PageRank search."
  (if (not (buffer-live-p orig-buf))
      "Error: Cannot locate original conversation buffer."
    (let* ((traces (with-current-buffer orig-buf
                     (macher-agent--buffer-to-traces (current-buffer))))
           (top-k (if (and ctx-lines (> ctx-lines 0)) ctx-lines 5))
           (kws (if (listp keywords) keywords (list keywords))))
      (if (null traces)
          (format "No matches found in history for: %s" (string-join kws ", "))
        (let* ((graph (macher-agent-zero-mem-build-graph traces))
               (retrieved (macher-agent-zero-mem-retrieve keywords graph :top-k top-k))
               (results nil))
          (dolist (tr retrieved)
            (let ((line (or (plist-get (macher-agent-zero-mem-trace-metadata tr) :line)
                            (macher-agent-zero-mem-trace-id tr)
                            (plist-get tr :id)))
                  (text (macher-agent-zero-mem-trace-text tr)))
              (push (format "--- Match near line %d ---\n%s\n" line text) results)))
          (if results
              (string-join (nreverse results) "\n")
            (format "No matches found in history for: %s" (string-join kws ", "))))))))

(defun macher-agent-zero-mem-install ()
  "Install zero-token memory hooks and dynamic pipeline steps."
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-memory-pipe--inject-tool 45)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-memory-pipe--truncate-buffer 55)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-zero-mem 75)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-memory-pipe--inject-directive 85)
  (add-hook 'macher-agent-task-flush-hook #'macher-agent-memory--persist-interaction)
  (setq macher-agent-search-backend-function #'macher-agent-memory-search-zero-mem))

(defun macher-agent-zero-mem-uninstall ()
  "Uninstall zero-token memory hooks and dynamic pipeline steps."
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-memory-pipe--inject-tool)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-memory-pipe--truncate-buffer)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-pipe--inject-zero-mem)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-memory-pipe--inject-directive)
  (remove-hook 'macher-agent-task-flush-hook #'macher-agent-memory--persist-interaction)
  (setq macher-agent-search-backend-function #'macher-agent-search-glob))

(provide 'macher-agent-zero-mem)
;;; macher-agent-zero-mem.el ends here
