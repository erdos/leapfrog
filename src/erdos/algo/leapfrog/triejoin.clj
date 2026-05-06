(ns erdos.algo.leapfrog.triejoin
 "Provides iterator protocols and implements a simple Leapfrog Triejoin algorithm on them."
 (:require [clojure.spec.alpha :as s]))


(defprotocol LinearIterator
  (->next [_] "Returns updated iterator positioned to the next value or nil when reached the end.")
  (get-key [_] "Returns current key of the iterator.")
  (->seek [_ k] "Returns iterator seeked to next key >= k if exists, nil otherwise."))


(defn iter-seq
"Returns a seq over elements of a LinearIterator"
[iterator]
(when iterator
  (cons (get-key iterator) (lazy-seq (iter-seq (->next iterator))))))

(defn- compare<= [a b] (<= (compare a b) 0))

;                HELPERS

(defn- subseq?
  [shorter longer]
  (cond (empty? shorter)                   true
        (empty? longer)                    false
        (= (first longer) (first shorter)) (recur (next shorter) (next longer) )
        :else                              (recur shorter (next longer))))


(defmacro ^:private reify' [metadata & methods]
  (letfn [(also-add [type & ms]
            (some->> methods (filter (comp (set ms) first)) (seq) (list* type)))]
  `(with-meta
    (reify
      clojure.lang.Sequential
      clojure.lang.Seqable
      (seq [this#] (iter-seq this#))
    
      clojure.lang.IReduceInit
      (reduce [this# f# start#]
              (transduce (comp (take-while some?) (map get-key))
                          f# start# (iterate ->next this#)))

     ~@(also-add 'LinearIterator '->next 'get-key '->seek)
     ~@(also-add 'TrieIterator 'trie-open)
     ~@(also-add 'Object 'toString)
     ~@(also-add 'clojure.lang.IDeref 'deref))
    ~metadata)))

;  Example implementation, only used for testing the algorithms

(defn sorted-iter
  "Builds a LinearIterator over the Sorted collection.
   Both Map and Set types are supported."
  [^clojure.lang.Sorted m]
  ((fn ctor [key-seq metadata]
     (when key-seq
        (reify' metadata
          (->next [this] (ctor (next key-seq) (meta this)))
          (get-key [_] (first key-seq))
          (->seek [this k]
            (ctor (seq (if (map? m) (map key (subseq m >= k)) (subseq m >= k)))
                  (meta this)))
          (toString [_] (str "<Iterator at " (first key-seq) "…>")))))
   (if (map? m) (seq (keys m)) (seq m))
   (meta m)))

;; Algorithm 2: leapfrog-search()
;;
;; Given a collection of iterators sorted by current keys, positions them to the next key
; that is common to all iterators, or nil when no such key found.
(defn- leapfrog-search [iterators]
  (assert (vector? iterators))
  (assert (not-empty iterators))
  ;; array of iterators must be sorted by current key.
  (let [size   (count iterators)
        perm-at #(mod % size)]
    (loop [iterators iterators
           pivot     0]
      (let [cur-iter (nth iterators (perm-at pivot))
            max-key  (get-key (nth iterators (perm-at (dec pivot))))]
        (if (= (get-key cur-iter) max-key)
          iterators
          (when-some [next-iter (->seek cur-iter max-key)]
            (recur (assoc iterators (perm-at pivot) next-iter)
                   (inc pivot))))))))


;; Algorithm 1: leapfrog-init()
;; Sorts iterators, positions them on the first common key.
(defn- leapfrog-init [iterators]
  (when (every? some? iterators)
    (leapfrog-search (vec (sort-by get-key iterators)))))


;; Algorithm 3: leapfrog-next()
;; Given all iterators on the same key, moves last one and then moves all to same key.
(defn- leapfrog-next [iterators]
  (when-let [max-iter (->next (last iterators))]
    (leapfrog-search (assoc iterators (dec (count iterators)) max-iter))))


;; Algorithm 4: leapfrog-seek()
;; Seeks last iterator to key then moves all iterators to same key.
(defn- leapfrog-seek [iterators seek-key]
  (when-let [max-iter (->seek (last iterators) seek-key)]
    (leapfrog-search (assoc iterators (dec (count iterators)) max-iter))))


;; Constructs a linear iterator that performs a leapfrog join on underlying linear iterators.
(defn leapfrog-join
  "Constructs a LinearIterator that performs a leapfrog join on the given iterators.
   The new iterator is Derefable, which yields the state of the underlying iterators."
  [iterators]
  ((fn ctor [iterators]
     (when iterators
       (reify' {}
          (->next [_]   (ctor (leapfrog-next iterators)))
          (get-key [_]  (get-key (first iterators)))
          (->seek [this k]
            (assert (<= (get-key this) k))
            (ctor (leapfrog-seek iterators k)))
          (deref [_] iterators)
          (toString [_] "<Leapfrog-Join>"))))
   (leapfrog-init iterators)))


(defprotocol TrieIterator
  (trie-open [_] "Proceed to the first key at the next depth")
  ; (trie-up) is not necessary, since the types are immutable.
  )


(defn union-iterator
  "Returns a TrieIterator that is the sorted union of the given iterators maps.
   All iterators must carry the same :variables metadata."
  [iterators]
  (assert (or (empty? iterators) (apply = (map :variables iterators))))
  ((fn ctor [iterators]
    (when (seq iterators)
      (reify' {}
        (->next [this]
          (let [min-key (get-key this)]
            (ctor (keep (fn [i] (if (= min-key (get-key i)) (->next i) i)) iterators))))
        (->seek [this k]
          (assert (<= (get-key this) k))
          (ctor (keep (fn [i] (if (compare<= k (get-key i)) i (->seek i k))) iterators)))
        (get-key [_] (reduce min (map get-key iterators)))      
        (trie-open [this]
        (let [min-key (get-key this)
              min-vec (filterv (fn [i] (= (get-key i) min-key)) iterators)]
          (ctor (keep trie-open min-vec))))
        (toString [_] "<Merge-Iterator>"))))
    (map :trie-iterator iterators)))


(defn union
  "Returns a relation map {:variables … :trie-iterator …} representing the
   union of the given relations. All relations must share the same variable list."
  [iterators]
  {:variables (:variables (first iterators))
   :trie-iterator (union-iterator iterators)})


(defn trie-iterator
  "Given a collection of tuples, builds a sorted nested trie and returns a
   TrieIterator positioned at the root's first key. Each tuple is a sequence
   of values defining a path from root to leaf through the trie."
  [relations]
  (let [trie (reduce (fn [trie rel]
                       ((fn g [trie p]
                          (when (seq p)
                            (update (or trie (sorted-map)) (first p) g (next p))))
                        trie rel))
                     (sorted-map)
                     relations)]
    (trie-open
     ((fn ctor [[current-level-seq :as stack] metadata]
        (reify' metadata
          (get-key [_]
            (when (seq current-level-seq)
              (key (first current-level-seq))))
          (->next [this]
            (let [n (next current-level-seq)]
              (when n (ctor (cons n (rest stack)) (meta this)))))
          (->seek [this k]
            (let [parent-map (if (second stack)
                                (val (first (second stack)))
                                trie)]
              (when-let [s (seq (subseq parent-map >= k))]
                (ctor (cons s (rest stack)) (meta this)))))
          (trie-open [this]
            ; open() moves to the first key at the next depth
            (let [child-map (val (first current-level-seq))]
              (when (map? child-map)
                ; If it's a leaf/value, we stay put or handle as needed
                (ctor (cons (seq child-map) stack) (meta this)))))
          (toString [_] (str "Path: " (mapv (comp key first) (reverse stack))))))
      (list (seq {nil trie}))
      (meta trie)))))


(defn trie-join-iterator
  "Constructs a TrieIterator implementing the Leapfrog Triejoin over relations
   under the given variable-ordering. Each relation must supply :variables (a
   subsequence of variable-ordering) and :trie-iterator. At each depth the
   algorithm performs a leapfrog join over the relations that share that depth's
   variable. Returns nil when no join result exists."
  [variable-ordering relations]
  (assert (every? #(subseq? % variable-ordering) (map :variables relations)))
  ((fn ctor [trie-iterators stack]
    (when (seq stack)
      ((fn ctor' [leapfrog-join-iter]
        (when leapfrog-join-iter
          (reify' {}
            ; The linear iterator methods are delegated to the leapfrog-join at the current depth.
            (->next [_]     (ctor' (->next leapfrog-join-iter)))
            (get-key [_]    (get-key leapfrog-join-iter))
            (->seek [_ key] (ctor' (->seek leapfrog-join-iter key)))
            (trie-open [_]
              (ctor (reduce (fn [m i] (assoc m (-> i meta ::index) (trie-open i))) trie-iterators @leapfrog-join-iter)
                    (next stack)))
            (toString [_] "<TrieJoin>"))))
      (leapfrog-join (map trie-iterators (first stack))))))
    (vec (map-indexed (fn [idx rel] (with-meta (:trie-iterator rel) {::index idx})) relations))
    (map (fn [v] (keep-indexed (fn [rel-idx relation] (when (some #{v} (:variables relation)) rel-idx)) relations))
         variable-ordering)))


(defn trie-join
  "Returns a relation map {:variables … :trie-iterator …} for the Leapfrog
   Triejoin of all given relations under variable-ordering."
  [variable-ordering relations]
  {:variables     variable-ordering
   :trie-iterator (trie-join-iterator variable-ordering relations)})


(defn trie-antijoin-iterator
  "Constructs a TrieIterator that is the antijoin of clause and not-clause.
   Each argument must be a map with :variables and :trie-iterator. The
   variables of not-clause must be a subsequence of clause's variables.
   Returns a TrieIterator over clause's tuples whose projection onto the
   shared variables matches no tuple in not-clause."
  [{cv :variables ci :trie-iterator}
   {ncv :variables nci :trie-iterator}]
  (assert (subseq? ncv cv))
  ((fn iters [ci cv nci ncv]
      (cond
        (nil? ci) nil
        (or (nil? nci) (empty? ncv)) ci

        (= (first cv) (first ncv))
        (let [rv (rest cv) rnv (rest ncv) leaf? (empty? rnv)]
          ((fn sync [ci nci]
             (let [node (fn [next-nci child]
                          (reify' (meta ci)
                             (->next [_]   (sync (->next ci) next-nci))
                             (get-key [_]  (get-key ci))
                             (->seek [_ k] (sync (->seek ci k) (->seek nci k)))
                             (trie-open [_] child)
                             (toString [_] "<Trie-Antijoin>")))]
               (cond
                 (nil? ci) nil
                 (nil? nci) ci
                 (= (get-key ci) (get-key nci))
                 (if leaf?
                   (recur (->next ci) (->next nci))
                   (if-let [ch (iters (trie-open ci) rv (trie-open nci) rnv)]
                     (node (->next nci) ch)
                     (recur (->next ci) (->next nci))))
                 (compare<= (get-key ci) (get-key nci))
                 (node nci (trie-open ci))
                 :else
                 (recur ci (->next nci)))))
           ci nci))

        :else
        ((fn pass [ci]
           (when ci
             (reify' (meta ci)
                (->next [_]   (pass (->next ci)))
                (get-key [_]  (get-key ci))
                (->seek [_ k] (pass (->seek ci k)))
                (trie-open [_] (iters (trie-open ci) (rest cv) nci ncv))
                (toString [_] "<Trie-Antijoin>"))))
         ci)))
    ci cv nci ncv))


(defn trie-antijoin
  "Returns a map {:variables … :trie-iterator …} that is the antijoin of
   clause with not-clause. The result inherits clause's variables."
  [clause not-clause]
  {:variables     (:variables clause)
   :trie-iterator (trie-antijoin-iterator clause not-clause)})


(defn dfs
  "Given a TrieIterator, returns a sequence of nodes in depth-first order."
  [iterator]
  (when iterator
    (cons (get-key iterator)
          (lazy-cat (dfs (trie-open iterator)) (dfs (->next iterator))))))


(defn bfs
  "Given a TrieIterator, returns a sequence of nodes in breadth-first order."
  [iterator]
  (->> (iterate ->next iterator)
       (take-while some?)
       (keep trie-open)
       (mapcat bfs)
       (lazy-cat (iter-seq iterator))))


(defn trie-routes
  "Given a TrieIterator, returns a lazy sequence of all complete paths (routes)
   as lists of keys from the current depth to the leaves."
  [iterator]
  (for [prefix (iterate ->next iterator)
        :while prefix
        tail (or (seq (trie-routes (trie-open prefix))) [[]])]
    (cons (get-key prefix) tail)))


(defn relations [{:keys [variables trie-iterator]}]
  (for [route (trie-routes trie-iterator)
        :when (= (count route) (count variables))]
    (zipmap variables route)))

#_
(defn singleton
  "Returns an iterator over a single value."
  [value]
  (reify' {}
    (->next [_] nil)
    (get-key [_] value)
    (->seek [t k] (when (= k value) t))
    (trie-open [_] nil)))


(defn seek-to
  "Tries to seek iterator to the given value, returns nil when value is not there."
  [iterator value]
  (assert (satisfies? LinearIterator iterator))
  (when-let [iterator (->seek iterator value)]
    (when (= (get-key iterator) value)
      iterator)))


(defn- reorder-1 [ctx new-var-order old-var-order old-iterator]
  (assert (map? ctx))
  (assert (= (set new-var-order)
             (into (set old-var-order) (keys ctx))))
  (assert (not-any? ctx old-var-order))
  (cond
     (empty? new-var-order) nil

     (= new-var-order old-var-order)
     old-iterator

     (= (first new-var-order) (first old-var-order))
     ((fn ctor [old-iterator]
        (when old-iterator
          (reify' (meta old-iterator)
            (get-key [_]  (get-key old-iterator))
            (->next [_]   (ctor (->next old-iterator)))
            (->seek [_ k] (ctor (->seek old-iterator k)))
            (trie-open [_]
              (reorder-1 ctx (next new-var-order) (next old-var-order) (trie-open old-iterator))))))
      old-iterator)

     (contains? ctx (first new-var-order))
     (reify' {}
      (get-key [_] (get ctx (first new-var-order)))
      (->next [_] nil)
      (->seek [_ k] (when (= k (get ctx (first new-var-order))) k))
      (trie-open [_] 
      ;; TODO: inline this check to the top of reorder-1
        (reorder-1 (dissoc ctx (first new-var-order))
                    (next new-var-order)
                    old-var-order
                    old-iterator)))

      :else
      (->> (iterate ->next old-iterator)
           (take-while some?)
           (keep #(reorder-1 (assoc ctx (first old-var-order) (get-key %))
                            new-var-order
                            (next old-var-order)
                            (trie-open %)))
           (map #(hash-map :variables new-var-order :trie-iterator %))
           (union-iterator))))


(defn reorder [new-var-order iter]
  {:variables new-var-order
   :trie-iterator (reorder-1 {} new-var-order (:variables iter) (:trie-iterator iter))})


(defn eager-iterator [{:keys [trie-iterator variables]}]
  ((fn ctor [iter ^long depth]
     (when iter
      (let [child (when (pos? depth) (ctor (trie-open iter) (dec depth)))]
        (if (and (pos? depth) (nil? child))
          (recur (->next iter) depth)
          (reify' (meta iter)
            (->next  [_]   (ctor (->next iter) depth))
            (get-key [_]   (get-key iter))
            (->seek  [_ k] (ctor (->seek iter k) depth))
            (trie-open [_] child)
            (toString [_] "<Eager>"))))))
      trie-iterator (dec (count variables))))


(defn eager
  "Returns a relation map whose trie-iterator emits only keys that participate
   in at least one complete path to a leaf at depth (count :variables).
   Useful for pruning phantom keys left by the lazy antijoin."
  [rel]
  {:variables (:variables rel)
   :trie-iterator (eager-iterator rel)})


(defn filtering-iterator [{:keys [variables trie-iterator]} filter-variables predicate]
  (assert (subseq? filter-variables variables))
  ((fn ctor [iter vars-rem filter-rem bindings]
     (cond
       (nil? iter) nil
       (empty? filter-rem) iter

       (and (= (first vars-rem) (first filter-rem))
            (nil? (next filter-rem))
            (not (apply predicate (conj bindings (get-key iter)))))
       (recur (->next iter) vars-rem filter-rem bindings)

       :else
       (reify' (meta iter)
         (->next  [_]   (ctor (->next iter) vars-rem filter-rem bindings))
         (get-key [_]   (get-key iter))
         (->seek  [_ k] (ctor (->seek iter k) vars-rem filter-rem bindings))
         (trie-open [_]
           (let [filter-here? (= (first vars-rem) (first filter-rem))]
             (ctor (trie-open iter) (next vars-rem)
                                    (if filter-here? (next filter-rem) filter-rem)
                                    (if filter-here? (conj bindings (get-key iter)) bindings))))
         (toString [_] "<Filtering>"))))
   trie-iterator variables filter-variables []))


(defn filtering
  "Returns a relation map {:variables … :trie-iterator …} that wraps rel,
   restricting the trie so a subtree under a path is only emitted when
   predicate returns true for the bindings of filter-variables along that
   path. filter-variables must be a subseq of rel's :variables and predicate
   arity matches filter-variables."
  [rel filter-variables predicate]
  {:variables     (:variables rel)
   :trie-iterator (filtering-iterator rel filter-variables predicate)})

