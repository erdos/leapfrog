(ns erdos.algo.leapfrog.triejoin-gen-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.set :as set]
            [erdos.algo.leapfrog.triejoin :refer :all]))

;; HELPERS

(defmacro for-props [bindings body]
  (let [bind-vars (vec (take-nth 2 bindings))]
    `(prop/for-all [~bind-vars (gen/let ~bindings ~bind-vars)]
      ~body)))

;; ---------------------------------------------------------------------------
;; Generators
;;
;; trie-iterator builds a trie from a collection of tuples; the result also
;; satisfies LinearIterator so it can be passed directly to leapfrog-join.
;; ---------------------------------------------------------------------------

(defn- gen-n-tuple-trie
  "Returns a generator that produces a relation map
   {:variables variables  :trie-iterator <TrieIterator>}
   built from random relations for supplied variables."
  [variables]
  {:pre [(seq variables) (every? keyword? variables)]}
  (->> (gen/set (gen/vector (gen/choose -10 10) (count variables))
                {:min-elements 1})
       (gen/fmap (partial into (sorted-set)))
       (gen/fmap trie-iterator)
       (gen/fmap (fn [iterator] {:variables variables :trie-iterator iterator}))))


(def gen-variables
  "Generates a non-empty vector of positional variable keywords [:v0 … :v(n-1)]."
  (gen/sized (fn [size] (->> (range (max 1 (min 6 size)))
                             (mapv #(keyword (str 'v %)))
                             (gen/return)))))

;; ---------------------------------------------------------------------------
;; Properties
;; ---------------------------------------------------------------------------

(defn get-tuples
  ([rel] (set (trie-routes (:trie-iterator rel))))
  ([rel n]
   (into #{} (filter (fn [route] (= n (count route))))
             (trie-routes (:trie-iterator rel)))))

(def tree->maps (comp set relations))

; Union of any number of tries with the same variables contains exactly all their tuples.
(defspec union-contains-all-tuples-from-all-relations
  100
  (for-props [variables gen-variables
              rels      (gen/not-empty (gen/vector (gen-n-tuple-trie variables)))]
    (= (get-tuples (union rels))
       (apply set/union (map get-tuples rels)))))


(defn- gen-covering-subsequences
  "Generates a non-empty vector of non-empty subsequences of `ordering` that
   together cover every element. Variables may appear in multiple subsequences."
  [ordering]
  (let [n (count ordering)]
    (gen/let [k       (gen/choose 1 n)
              ;; Each variable has one mandatory bucket (guarantees coverage)
              primary (apply gen/tuple (repeat n (gen/choose 0 (dec k))))
              ;; Each variable may also appear in additional buckets
              extras  (apply gen/tuple (repeat n (gen/set (gen/choose 0 (dec k)))))]
      (->> (range k)
           (keep (fn [bucket]
                   (not-empty
                    (vec (keep-indexed (fn [i v]
                                        (when (or (= bucket (nth primary i))
                                                  (contains? (nth extras i) bucket))
                                          v))
                                      ordering)))))
           vec))))


(defn- naive-join [variable-ordering relations]
  (->> relations
       (map tree->maps)
       (reduce set/join)
       (map (fn [m] (mapv m variable-ordering)))
       (into (sorted-set))))


(defspec trie-join-matches-naive-join
  500
  (for-props [ordering gen-variables
              subseqs  (gen-covering-subsequences ordering)
              rels     (apply gen/tuple (map gen-n-tuple-trie subseqs))]
    (= (get-tuples (trie-join ordering rels) (count ordering))
       (naive-join ordering rels))))


(defn- gen-subsequence-of
  "Returns a generator that produces a non-empty subsequence of ordering,
   preserving the original relative order."
  [ordering]
  (gen/fmap (fn [indices] (mapv #(nth ordering %) (sort indices)))
            (gen/not-empty (gen/set (gen/choose 0 (dec (count ordering)))))))


(defn- naive-antijoin [clause not-clause]
  (let [clause-vars     (:variables clause)
        not-clause-vars (:variables not-clause)
        shared-vars     (vec (filter (set not-clause-vars) clause-vars))
        not-clause-set  (->> (trie-routes (:trie-iterator not-clause))
                             (map (fn [t] (zipmap not-clause-vars t)))
                             (map (fn [m] (select-keys m shared-vars)))
                             (set))]
    (->> (trie-routes (:trie-iterator clause))
         (filter (fn [t]
                   (let [m (zipmap clause-vars t)
                         shared-m (select-keys m shared-vars)]
                     (not (contains? not-clause-set shared-m)))))
         (set))))


(defspec trie-antijoin-matches-naive-antijoin
  100
  (for-props [clause-vars     gen-variables
              clause          (gen-n-tuple-trie clause-vars)
              not-clause-vars (gen-subsequence-of clause-vars)
              not-clause      (gen-n-tuple-trie not-clause-vars)]
    (= (get-tuples (trie-antijoin clause not-clause) (count clause-vars))
       (naive-antijoin clause not-clause))))


(defspec reorder-preserves-tuples
  1000
  (for-props [variables gen-variables
              rel       (gen-n-tuple-trie variables)
              perm      (gen/shuffle variables)]
    (= (tree->maps rel)
       (tree->maps (reorder perm rel)))))


(defspec filtering-matches-naive-filter
  1000
  (for-props [variables   gen-variables
              filter-vars (gen-subsequence-of variables)
              rel         (gen-n-tuple-trie variables)]
    (= (->> (relations rel)
            (filter (fn [m] (even? (reduce + (map m filter-vars))))))
       (->> (filtering rel filter-vars (fn [& xs] (even? (reduce + xs))))
            (relations)))))

