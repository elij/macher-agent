;;; macher-agent-zero-mem.el --- Zero-Token Memory Operations for macher-agent -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Zero-Token Memory (Zero-Mem) plugin for macher-agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'text-property-search)
(require 'macher-agent-core)
(require 'macher-agent-presets)
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
  "Ratio of document transitions vs temporal neighbours (E_dd)."
  :type 'float
  :group 'macher-agent-zero-mem)

(defcustom macher-agent-zero-mem-stop-words
  '("The" "This" "That" "They" "Here" "There" "What" "When" "How" "Why" "Who"
    "But" "And" "For" "With" "From" "Your" "I" "We" "You" "It" "An" "If" "Or"
    "As" "To" "In" "At" "Of" "On" "By" "About" "An" "Be" "Is" "Are" "Was" "Were"
    "Been" "Has" "Have" "Had" "Do" "Does" "Did" "Will" "Would" "Should" "Can")
  "List of capitalisation-prone common stop words to filter out during NER."
  :type '(repeat string)
  :group 'macher-agent-zero-mem)

;;;; Naive Named Entity Recognition (NER)

(defun macher-agent-zero-mem-naive-ner (text)
  "Extract entity strings from TEXT using deterministic heuristic rules.
Return a deduplicated list of lowercase strings representing the entities."
  (let ((case-fold-search nil)
        (entities nil))
    (with-temp-buffer
      (insert (or text ""))

      ;; Multi-word or single proper nouns
      (goto-char (point-min))
      (while (re-search-forward "\\b\\([A-Z][a-zA-Z0-9-]+\\(?: [A-Z][a-zA-Z0-9-]+\\)*\\)\\b" nil t)
        (let ((match (match-string 1)))
          (unless (member match macher-agent-zero-mem-stop-words)
            (push (downcase match) entities))))

      ;; Technical and code identifiers
      (goto-char (point-min))
      (while (re-search-forward "\\b\\([a-z]+-[a-z0-9-]+\\|[a-z]+_[a-z0-9_]+\\|[A-Z][A-Za-z0-9_]+[a-z][A-Za-z0-9_]*\\)\\b" nil t)
        (push (downcase (match-string 1)) entities))

      ;; Exact quoted strings
      (goto-char (point-min))
      (while (re-search-forward "\"\\([^\"]+\\)\"" nil t)
        (push (downcase (match-string 1)) entities)))

    (delete-dups entities)))

;;;; Bipartite Graph Construction and Weighting

(cl-defstruct (macher-agent-zero-mem-trace
               (:constructor macher-agent-zero-mem-trace-create))
  "Represent an individual context unit or document node (V_d)."
  id
  text
  timestamp
  entities
  metadata)

(cl-defstruct (macher-agent-zero-mem-graph
               (:constructor macher-agent-zero-mem-graph-create))
  "Represent the bipartite relational trace graph."
  traces
  entity-index
  adj-list
  modified-tick
  (content-hash nil))

(defun macher-agent-zero-mem-build-graph (traces-raw &optional modified-tick content-hash)
  "Build the relational trace graph from a list of raw trace plists TRACES-RAW.
MODIFIED-TICK is the optional buffer modification tick integer.
CONTENT-HASH is the optional content hash integer.
Return a `macher-agent-zero-mem-graph' struct."
  (let* ((traces (make-hash-table :test 'equal))
         (entity-index (make-hash-table :test 'equal))
         (adj-list (make-hash-table :test 'equal))
         (trace-list nil)
         (id-counter 0))

    (dolist (raw traces-raw)
      (let* ((text (or (plist-get raw :content) (plist-get raw :text)))
             (ts (or (plist-get raw :timestamp) (float id-counter)))
             (meta (let ((m (copy-sequence (plist-get raw :metadata))))
                     (if (plist-get raw :archived)
                         (plist-put m :archived t)
                       m)))
             (proposed-id (or (plist-get raw :id) id-counter))
             (raw-id (if (gethash proposed-id traces)
                         (progn
                           (while (gethash id-counter traces)
                             (setq id-counter (1+ id-counter)))
                           id-counter)
                       proposed-id))
             (ents (macher-agent-zero-mem-naive-ner text))
             (ents (or ents
                       (cl-remove-if (lambda (w) (or (< (length w) 2)
                                                     (member (capitalize w) macher-agent-zero-mem-stop-words)))
                                     (delete-dups (split-string (downcase (or text "")) "[^[:alnum:]-_]+" t)))))
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

    (dolist (trace trace-list)
      (let ((tid (macher-agent-zero-mem-trace-id trace)))
        (dolist (ent (macher-agent-zero-mem-trace-entities trace))
          (puthash ent (cons tid (gethash ent entity-index)) entity-index))))

    (let ((n-traces (length trace-list)))
      (dotimes (i n-traces)
        (let* ((trace (nth i trace-list))
               (tid (macher-agent-zero-mem-trace-id trace))
               (ents (macher-agent-zero-mem-trace-entities trace))
               (e-weight-sum (float (length ents)))
               (doc-node (cons :doc tid))
               (doc-transitions nil))

          (when (> e-weight-sum 0.0)
            (dolist (ent ents)
              (let* ((raw-w (/ 1.0 e-weight-sum))
                     (scaled-w (* macher-agent-zero-mem-entity-transition-ratio raw-w)))
                (push (cons (cons :ent ent) scaled-w) doc-transitions))))

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
     :adj-list adj-list
     :modified-tick modified-tick
     :content-hash content-hash)))

;;;; Query Alignment and PageRank Diffusion

(defun macher-agent-zero-mem-align-query (query &optional graph)
  "Generate explicit alignment offsets based on QUERY and optional GRAPH."
  (when (stringp query)
    (setq query (split-string (downcase query) "[^[:alnum:]-_]+" t)))
  (cl-check-type query list)
  (if (null graph)
      query
    (let* ((case-fold-search t)
           (entity-index (macher-agent-zero-mem-graph-entity-index graph))
           (q-str (string-join (mapcar (lambda (x) (format "%s" x)) query) " "))
           (q-entities (or (macher-agent-zero-mem-naive-ner q-str)
                           query))
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
      aligned-activations)))

(defconst macher-agent-zero-mem-pr-scale 65536
  "Fixed-point scaling factor for PageRank.")

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
  "Compile ADJ-LIST into a vector of transitions using fixed-point weights."
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
  "Initialise vectorised reset distribution for QUERY on GRAPH."
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
  "Execute vectorised PageRank iteration on CTX."
  (let* ((count (macher-agent-zero-mem-pr-ctx-node-count ctx))
         (pi-array (macher-agent-zero-mem-pr-ctx-pi-array ctx))
         (next-array (macher-agent-zero-mem-pr-ctx-next-array ctx))
         (reset-array (macher-agent-zero-mem-pr-ctx-reset-array ctx))
         (transitions (macher-agent-zero-mem-pr-ctx-transitions ctx)))
    (dotimes (i count)
      (aset next-array i (ash (* one-minus-gamma-fp (aref reset-array i)) -16)))
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
  "Compute Stationary Personalised PageRank for QUERY on GRAPH."
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
        (dotimes (i count)
          (aset pi-array i (aref reset-array i)))
        (dotimes (_ iters)
          (macher-agent-zero-mem--pr-tick ctx gamma-fp one-minus-gamma-fp)
          (macher-agent-zero-mem--swap-arrays ctx))
        (macher-agent-zero-mem--export-scores ctx)))))

;;;; Dual-View Evidence Retrieval and Fusion

(defun macher-agent-zero-mem-retrieve (query graph &rest args)
  "Execute Dual-View Evidence retrieval for QUERY on GRAPH.
ARGS is a property list supporting `:top-k', `:iterations', `:max-line',
and `:max-offset'."
  (let ((top-k (or (plist-get args :top-k) 5))
        (iterations (or (plist-get args :iterations) 15))
        (max-line (plist-get args :max-line))
        (max-offset (plist-get args :max-offset)))
    (when (and graph (macher-agent-zero-mem-align-query query graph))
      (let* ((pi-table (macher-agent-zero-mem-pagerank-fixed-point query graph iterations))
             (doc-scores nil)
             (traces-ht (macher-agent-zero-mem-graph-traces graph)))
        (maphash
         (lambda (node score)
           (when (eq (car node) :doc)
             (push (cons (cdr node) score) doc-scores)))
         pi-table)
        (setq doc-scores (sort doc-scores (lambda (a b) (> (cdr a) (cdr b)))))
        (let ((results nil))
          (cl-loop for (id . _score) in doc-scores
                   while (< (length results) top-k)
                   do (when-let* ((trace (if (hash-table-p traces-ht)
                                             (gethash id traces-ht)
                                           (when (and (vectorp traces-ht) (< id (length traces-ht)))
                                             (aref traces-ht id)))))
                        (let* ((meta (macher-agent-zero-mem-trace-metadata trace))
                               (archived (plist-get meta :archived))
                               (line (or (plist-get meta :line)
                                         (macher-agent-zero-mem-trace-id trace)))
                               (offset (plist-get meta :offset))
                               (within-bounds (or archived
                                                  (and (or (null max-line) (null line) (< line max-line))
                                                       (or (null max-offset) (null offset) (< offset max-offset))))))
                          (when within-bounds
                            (push trace results)))))
          (nreverse results))))))

;;;; Event Horizon and Trace Indexing

(defvar-local macher-agent-zero-mem--event-horizon nil
  "Buffer-local event horizon plist (:line L :offset O :tick T).")

(defun macher-agent-zero-mem--calculate-event-horizon (orig-buf)
  "Extract event index marker explicitly from ORIG-BUF live state."
  (cl-check-type orig-buf buffer)
  (when (buffer-live-p orig-buf)
    (with-current-buffer orig-buf
      (let ((max-chars (macher-agent--get-max-context-chars orig-buf))
            (tick (buffer-chars-modified-tick orig-buf)))
        (if (<= (buffer-size) max-chars)
            (list :line 1 :offset (point-min) :tick tick)
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
                    :offset safe-pt
                    :tick tick))))))))

(defun macher-agent-zero-mem--get-event-horizon (orig-buf &optional context)
  "Correlate index marker linking ORIG-BUF object against optional CONTEXT struct."
  (cl-check-type orig-buf buffer)
  (when (buffer-live-p orig-buf)
    (let* ((current-tick (buffer-chars-modified-tick orig-buf))
           (local-horizon (buffer-local-value 'macher-agent-zero-mem--event-horizon orig-buf))
           (ctx-horizon (when (macher-agent-valid-context-p context)
                          (plist-get (macher-agent-context-plugins context) :event-horizon))))
      (cond
       ((and local-horizon
             (or (null (plist-get local-horizon :tick))
                 (eq (plist-get local-horizon :tick) current-tick)))
        local-horizon)
       ((and ctx-horizon
             (or (null (plist-get ctx-horizon :tick))
                 (eq (plist-get ctx-horizon :tick) current-tick)))
        ctx-horizon)
       (t
        (macher-agent-zero-mem--calculate-event-horizon orig-buf))))))

(defun macher-agent-zero-mem--buffer-to-traces (buffer &optional before-offset before-line)
  "Format continuous background logs via live BUFFER object."
  (cl-check-type buffer buffer)
  (when (buffer-live-p buffer)
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
              (unless (or (string-empty-p (string-trim line-text))
                          (string-match-p "\\[\\.\\.\\. SYSTEM ALERT: macher-agent truncated" line-text))
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

;;;; Plugin State Accessors and Graph Cache

(defun macher-agent-zero-mem-get-state (context)
  "Retrieve mapping registry bounds explicitly from CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (plist-get (macher-agent-context-plugins context) :zero-mem))

(defun macher-agent-zero-mem-set-state (context state)
  "Update mapping registry bounds natively against CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (setf (macher-agent-context-plugins context)
        (plist-put (copy-sequence (macher-agent-context-plugins context)) :zero-mem state))
  state)

(defvar macher-agent-memory-vector-storage (make-hash-table :test 'equal)
  "Vector storage repository committing interaction histories and trace graphs.")

(defun macher-agent-zero-mem--get-graph (orig-buf &optional context)
  "Retrieve or re-index the full trace graph for ORIG-BUF, validating modification ticks and content hashes."
  (let ((buf (cond ((bufferp orig-buf) orig-buf)
                   ((stringp orig-buf) (get-buffer orig-buf))
                   (t nil))))
    (when (and buf (buffer-live-p buf))
      (let* ((buf-name (buffer-name buf))
             (ctx (or (when (macher-agent-valid-context-p context) context)
                      (macher-agent-resolve-context buf)))
             (current-tick (buffer-chars-modified-tick buf))
             (current-hash (with-current-buffer buf
                             (sxhash-equal (buffer-substring-no-properties (point-min) (point-max)))))
             (cached-graph (or (when ctx (macher-agent-zero-mem-get-state ctx))
                               (gethash buf-name macher-agent-memory-vector-storage))))
        (if (and cached-graph
                 (eql (macher-agent-zero-mem-graph-modified-tick cached-graph) current-tick)
                 (eql (macher-agent-zero-mem-graph-content-hash cached-graph) current-hash))
            cached-graph
          (let* ((prev-graph (or cached-graph (gethash buf-name macher-agent-memory-vector-storage)))
                 (raw-traces (macher-agent-zero-mem--buffer-to-traces buf))
                 (new-texts (make-hash-table :test 'equal))
                 (archived-raw-traces nil))
            (remhash buf-name macher-agent-memory-vector-storage)
            (when ctx
              (macher-agent-zero-mem-set-state ctx nil))
            ;; Record active texts in newly parsed traces
            (dolist (raw raw-traces)
              (let ((txt (or (plist-get raw :content) (plist-get raw :text))))
                (when txt
                  (puthash (string-trim txt) t new-texts))))
            ;; When previous graph exists, preserve trace nodes not in newly parsed raw-traces
            (when prev-graph
              (let ((prev-traces
                     (let ((pt (macher-agent-zero-mem-graph-traces prev-graph)))
                       (cond
                        ((hash-table-p pt)
                         (let (list)
                           (maphash (lambda (_k v) (push v list)) pt)
                           (sort list (lambda (a b)
                                        (< (or (ignore-errors (macher-agent-zero-mem-trace-timestamp a))
                                               (ignore-errors (macher-agent-zero-mem-trace-id a)) 0)
                                           (or (ignore-errors (macher-agent-zero-mem-trace-timestamp b))
                                               (ignore-errors (macher-agent-zero-mem-trace-id b)) 0))))))
                        ((vectorp pt) (append pt nil))
                        ((listp pt) pt))))
                    (seen-archived (make-hash-table :test 'equal)))
                (dolist (tr prev-traces)
                  (let* ((txt (or (when (macher-agent-zero-mem-trace-p tr)
                                    (macher-agent-zero-mem-trace-text tr))
                                  (plist-get tr :content)
                                  (plist-get tr :text)
                                  (when (and (listp tr) (>= (length tr) 2) (stringp (nth 1 tr)))
                                    (nth 1 tr))))
                         (trimmed (when txt (string-trim txt))))
                    (when (and trimmed
                               (not (gethash trimmed new-texts))
                               (not (gethash trimmed seen-archived)))
                      (puthash trimmed t seen-archived)
                      (let* ((meta (let ((m (copy-sequence
                                             (or (when (macher-agent-zero-mem-trace-p tr)
                                                   (macher-agent-zero-mem-trace-metadata tr))
                                                 (plist-get tr :metadata)))))
                                     (plist-put m :archived t)))
                             (id (or (when (macher-agent-zero-mem-trace-p tr)
                                       (macher-agent-zero-mem-trace-id tr))
                                     (plist-get tr :id)))
                             (ts (or (when (macher-agent-zero-mem-trace-p tr)
                                       (macher-agent-zero-mem-trace-timestamp tr))
                                     (plist-get tr :timestamp))))
                        (push (list :id id
                                    :text txt
                                    :content txt
                                    :timestamp ts
                                    :archived t
                                    :metadata meta)
                              archived-raw-traces)))))))
            (setq archived-raw-traces (nreverse archived-raw-traces))
            (let* ((all-traces (append archived-raw-traces raw-traces))
                   (new-graph (when all-traces
                                (macher-agent-zero-mem-build-graph all-traces current-tick current-hash))))
              (when new-graph
                (puthash buf-name new-graph macher-agent-memory-vector-storage)
                (when ctx
                  (macher-agent-zero-mem-set-state ctx new-graph)))
              new-graph)))))))

(defun macher-agent-memory--persist-interaction (&optional context-or-buffer)
  "Commit conversation history in CONTEXT-OR-BUFFER to vector storage."
  (let* ((ctx (macher-agent-resolve-context context-or-buffer))
         (buf (cond
               ((bufferp context-or-buffer) context-or-buffer)
               ((stringp context-or-buffer) (get-buffer context-or-buffer))
               ((and ctx (macher-agent-context-origin-buffer ctx))
                (macher-agent-context-origin-buffer ctx))
               (t (current-buffer)))))
    (when (and buf (buffer-live-p buf))
      (macher-agent-zero-mem--get-graph buf ctx))))

;;;; Buffer and Context Resolution Helpers

(defun macher-agent-zero-mem--extract-clean-prompt (orig-buf &optional context)
  "Extract the current user prompt string from CONTEXT or ORIG-BUF."
  (cl-check-type orig-buf buffer)
  (let* ((ctx (or (when (macher-agent-valid-context-p context) context)
                  (macher-agent-resolve-context orig-buf)))
         (raw-prompt (or (when ctx (macher-agent-context-prompt ctx))
                         (when (buffer-live-p orig-buf)
                           (with-current-buffer orig-buf
                             (let ((start (or (previous-single-property-change (point-max) 'gptel)
                                              (point-min))))
                               (buffer-substring-no-properties start (point-max))))))))
    (when (and raw-prompt (stringp raw-prompt))
      (let ((stripped (replace-regexp-in-string "<!--[[:space:]\n]*Local Variables:[^>]*-->" "" raw-prompt)))
        (string-trim (replace-regexp-in-string "^[#>*[:space:]]+" "" (string-trim stripped)))))))

(defun macher-agent-zero-mem--resolve-parent-buffer (&optional arg1 arg2 &rest _rest)
  "Resolve parent orchestrator buffer from ARG1 and ARG2 (buffer or context)."
  (let* ((context (cond ((macher-agent-context-p arg1) arg1)
                        ((macher-agent-context-p arg2) arg2)
                        (t nil)))
         (target-buf (cond ((bufferp arg1) arg1)
                           ((bufferp arg2) arg2)
                           ((stringp arg1) (get-buffer arg1))
                           ((stringp arg2) (get-buffer arg2))
                           (t (current-buffer))))
         (resolved-ctx (or context (macher-agent-resolve-context target-buf))))
    (or (when (and target-buf (bufferp target-buf) (buffer-live-p target-buf))
          (with-current-buffer target-buf
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
                   (t nil)))))))
        (when resolved-ctx
          (let* ((orig (or (macher-agent-context-origin-buffer resolved-ctx)
                           (plist-get (macher-agent-context-plugins resolved-ctx) :origin-buffer)
                           (plist-get (macher-agent-context-plugins resolved-ctx) :originator-buffer)))
                 (orig-buf (cond ((bufferp orig) (when (buffer-live-p orig) orig))
                                 ((stringp orig) (get-buffer orig))
                                 (t nil))))
            (when (and orig-buf (not (eq orig-buf target-buf)))
              orig-buf))))))

;;;; Transmission Pipeline Steps

(defun macher-agent-memory-pipe--inject-tool (state)
  "Inject memory tools into transmission STATE struct when buffer exceeds `max-chars'."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((buf (or (macher-agent-transmission-state-target-buffer state)
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
                                         (equal (macher-agent-canonical-tool-name tl)
                                                "search_conversation_history"))
                                       tools)))
        (unless already-has-mem
          (setf (macher-agent-transmission-state-tools state)
                (append tools (list mem-tool)))))))
  state)

(defun macher-agent-parent-memory-pipe--inject-tool (state)
  "Inject parent memory tools into transmission STATE struct."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((ctx (macher-agent-transmission-state-context state))
         (target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (parent (macher-agent-zero-mem--resolve-parent-buffer target-buf ctx)))
    (when (and parent (buffer-live-p parent))
      (let* ((tools (macher-agent-transmission-state-tools state))
             (parent-tool (or (ignore-errors (macher-agent-resolve-tool "search_parent_conversation_history" nil nil nil))
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

(defun macher-agent-memory-pipe--truncate-buffer (state)
  "Truncate transmission wire buffer defined in STATE struct."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((tx-buf (current-buffer))
         (ref-buf (or (macher-agent-transmission-state-target-buffer state) tx-buf))
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
            (let* ((lines-deleted (count-lines safe-min safe-pt))
                   (horizon-line (line-number-at-pos safe-pt))
                   (target-buf (or (macher-agent-transmission-state-target-buffer state) ref-buf)))
              (when (and target-buf (buffer-live-p target-buf))
                ;; Ensure target-buf's current history is indexed in vector storage before truncation
                (macher-agent-zero-mem--get-graph target-buf)
                (with-current-buffer target-buf
                  (setq-local macher-agent-zero-mem--event-horizon
                              (list :line horizon-line
                                    :offset safe-pt
                                    :tick (buffer-chars-modified-tick target-buf)))))
              (delete-region safe-min safe-pt)
              (goto-char safe-min)
              (insert (format "\n[... SYSTEM ALERT: macher-agent truncated %d lines of early history to conserve tokens. If you need this context, use the `search_conversation_history` tool ...]\n\n" lines-deleted))
              (when (looking-at "^[ \t\n\r]+")
                (replace-match ""))))))))
  state)

(defun macher-agent-pipe--inject-zero-mem (state)
  "Inject zero-mem prompt sequences into STATE struct for truncated history."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((buf (or (macher-agent-transmission-state-target-buffer state)
                  (current-buffer)))
         (context (or (macher-agent-transmission-state-context state)
                      (macher-agent-resolve-context buf)))
         (horizon (when (buffer-live-p buf)
                    (macher-agent-zero-mem--get-event-horizon buf context)))
         (horizon-line (plist-get horizon :line))
         (horizon-offset (plist-get horizon :offset))
         (is-truncated (and (buffer-live-p buf)
                            horizon-offset
                            (> horizon-offset (with-current-buffer buf (point-min)))))
         (graph (when (and is-truncated (buffer-live-p buf))
                  (macher-agent-zero-mem--get-graph buf context)))
         (query (when (and is-truncated (buffer-live-p buf))
                  (macher-agent-zero-mem--extract-clean-prompt buf context))))
    (when (and is-truncated graph query (not (string-empty-p query)))
      (let* ((traces (macher-agent-zero-mem-retrieve
                      query graph
                      :top-k 5
                      :max-line horizon-line
                      :max-offset horizon-offset))
             (formatted-traces nil))
        (dolist (tr traces)
          (let ((line (or (plist-get (macher-agent-zero-mem-trace-metadata tr) :line)
                          (macher-agent-zero-mem-trace-id tr)))
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

(defun macher-agent-pipe--inject-parent-context (state)
  "Inject parent references into STATE struct."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((ctx (macher-agent-transmission-state-context state))
         (target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (parent (macher-agent-zero-mem--resolve-parent-buffer target-buf ctx)))
    (when (and parent (buffer-live-p parent))
      (let* ((parent-ctx (macher-agent-resolve-context parent))
             (parent-graph (macher-agent-zero-mem--get-graph parent parent-ctx))
             (query (macher-agent-zero-mem--extract-clean-prompt target-buf ctx)))
        (when (and parent-graph query (not (string-empty-p query)))
          (let* ((traces (macher-agent-zero-mem-retrieve query parent-graph :top-k 5))
                 (formatted-traces nil))
            (dolist (tr traces)
              (let ((line (or (plist-get (macher-agent-zero-mem-trace-metadata tr) :line)
                              (macher-agent-zero-mem-trace-id tr)))
                    (text (macher-agent-zero-mem-trace-text tr)))
                (push (format "<trace id=\"%s\">\n%s\n</trace>" line text) formatted-traces)))
            (when formatted-traces
              (let* ((traces-content (string-join (nreverse formatted-traces) "\n"))
                     (evidence-block (format "<parent_conversation_context>\n%s\n</parent_conversation_context>" traces-content)))
                (setf (macher-agent-transmission-state-directives state)
                      (append (macher-agent-transmission-state-directives state)
                              (list evidence-block)))))))))
    state))

(defun macher-agent-memory-pipe--inject-directive (state)
  "Append search directive to STATE if memory tools are active."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((tools (macher-agent-transmission-state-tools state))
         (has-mem (cl-some (lambda (tl)
                             (equal (macher-agent-canonical-tool-name tl)
                                    "search_conversation_history"))
                           tools)))
    (when has-mem
      (let ((directive "CRITICAL DIRECTIVE: Early conversation history has been truncated. You MUST use the `search_conversation_history` tool to retrieve missing context if necessary."))
        (push directive (macher-agent-transmission-state-directives state)))))
  state)

(defun macher-agent-parent-memory-pipe--inject-directive (state)
  "Append parent search directive to STATE if parent search tool is active."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((tools (macher-agent-transmission-state-tools state))
         (has-parent-tool (cl-some (lambda (tl)
                                     (equal (macher-agent-canonical-tool-name tl)
                                            "search_parent_conversation_history"))
                                   tools)))
    (when has-parent-tool
      (let ((directive "CRITICAL DIRECTIVE: You are a specialised child sub-agent spawned to execute a specific, narrow task. Your sole objective is to complete your assigned prompt. You have read-only access to the parent orchestrator's memory via the `search_parent_conversation_history` tool. Treat the parent's history strictly as a reference archive to extract missing variables, definitions, or background facts required for your specific sub-task. Leave all orchestration, task delegation, and overarching goals to the parent."))
        (push directive (macher-agent-transmission-state-directives state)))))
  state)

;;;; Search Backend with Event Horizon Filtering

(defun macher-agent-memory-search-zero-mem (orig-buf keywords &optional ctx-lines)
  "Execute memory recall sequence on ORIG-BUF for KEYWORDS."
  (cl-check-type orig-buf buffer)
  (cl-check-type keywords (or string list))
  (let ((kws (if (listp keywords) keywords (list keywords))))
    (if (not (buffer-live-p orig-buf))
        "Error: Cannot locate original conversation buffer."
      (let* ((top-k (if (and ctx-lines (> ctx-lines 0)) ctx-lines 5))
             (context (macher-agent-resolve-context orig-buf))
             (is-self-query (eq orig-buf (current-buffer)))
             (horizon (when is-self-query
                        (macher-agent-zero-mem--get-event-horizon orig-buf context)))
             (horizon-line (plist-get horizon :line))
             (horizon-offset (plist-get horizon :offset))
             (is-truncated (and is-self-query
                                horizon-offset
                                (> horizon-offset (with-current-buffer orig-buf (point-min)))))
             (graph (macher-agent-zero-mem--get-graph orig-buf context))
             (has-archived (when graph
                             (let ((found nil)
                                   (traces (macher-agent-zero-mem-graph-traces graph)))
                               (when (hash-table-p traces)
                                 (maphash (lambda (_k tr)
                                            (when (plist-get (macher-agent-zero-mem-trace-metadata tr) :archived)
                                              (setq found t)))
                                          traces))
                               found))))
        (if (or (null graph) (and is-self-query (not is-truncated) (not has-archived)))
            (format "No matches found in history for: %s" (string-join kws ", "))
          (let* ((retrieved (macher-agent-zero-mem-retrieve
                             kws graph
                             :top-k top-k
                             :max-line (when is-truncated horizon-line)
                             :max-offset (when is-truncated horizon-offset)))
                 (results nil))
            (dolist (tr retrieved)
              (let* ((meta (macher-agent-zero-mem-trace-metadata tr))
                     (line (or (plist-get meta :line)
                               (macher-agent-zero-mem-trace-id tr)))
                     (text (macher-agent-zero-mem-trace-text tr)))
                (push (format "--- Match near line %d ---\n%s\n" line text) results)))
            (if results
                (string-join (nreverse results) "\n")
              (format "No matches found in history for: %s" (string-join kws ", ")))))))))

;;;; Plugin Lifecycle Management

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

(defun macher-agent-clear-zero-mem (&optional target-buf)
  "Clear Zero-Mem graph, entity mappings, and vector storage for TARGET-BUF."
  (interactive)
  (let* ((buf (or target-buf (current-buffer)))
         (buf-name (when (buffer-live-p buf) (buffer-name buf)))
         (context (macher-agent-resolve-context buf)))
    (when (macher-agent-valid-context-p context)
      (macher-agent-zero-mem-set-state context nil))
    (when (and buf-name (hash-table-p macher-agent-memory-vector-storage))
      (remhash buf-name macher-agent-memory-vector-storage))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local macher-agent-zero-mem--event-horizon nil)))
    (message "Macher-Agent: Zero-Mem graph and vector storage cleared for '%s'." (or buf-name "buffer"))))

(provide 'macher-agent-zero-mem)
;;; macher-agent-zero-mem.el ends here
