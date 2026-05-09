(ns erdos.algo.leapfrog.triejoin-test
  (:require [clojure.test :refer [deftest testing is are]]
            [erdos.algo.leapfrog.triejoin :as sut :refer :all]))

(deftest subseq?-tests
  (def subseq? @#'sut/subseq?)
  (testing "Trivial cases"
    (is (subseq? [] []))
    (is (subseq? nil nil))
    (is (subseq? [] [1 2]))
    (is (subseq? [1 2 3] [1 2 3])))
  (is (subseq? [2 3] [1 2 3 4]))
  (is (subseq? [1 4] [1 2 3 4]))
  (is (not (subseq? [0 1 2] [1 2 3 4])))
  (is (not (subseq? [3 4 5] [1 2 3 4])))
  (is (not (subseq? [1] []))))

(deftest sorted-iter-tests
  (let [m (sorted-iter (sorted-set 1 2 3 4 5))]
    (is (= 1 (get-key m)))
    (is (= 2 (-> m ->next get-key)))
    (is (= 3 (-> m ->next ->next get-key)))
    (is (= 1 (-> m (->seek -1) (get-key))))
    (is (= 2 (-> m (->seek 2) (get-key))))
    (is (= 3 (-> m (->seek 2.1) (get-key))))
    (is (nil? (->seek m 123)))
    (is (= [1 2 3 4 5] (vec m)))
    (is (= 15 (reduce + 0 m)))))

(deftest leapfrog-join-tests
  (let [rel-1 (sorted-iter (sorted-set 2 4 6 8 10 12 14 16 18))
        rel-2 (sorted-iter (sorted-set 1 2 3 4 5 6 7 8 9 10))
        rel-3 (sorted-iter (sorted-set 1 2 3 5 8 13 21 34))]
    (is (= [2 4 6 8 10] (vec (leapfrog-join [rel-1 rel-2]))))
    (is (= [1 2 3 5 8] (vec (leapfrog-join [rel-2 rel-3]))))
    (is (= [2 8] (vec (leapfrog-join [rel-1 rel-3]))))
    (is (= [2 8] (vec (leapfrog-join [rel-1 rel-2 rel-3]))))
    (is (= nil (leapfrog-join [rel-1 (sorted-iter (sorted-set -1))])))
    #_(is (map? (meta (leapfrog-join [rel-1 rel-2])))))

  (testing "leapfrog-join over keyword-keyed iterators, including ->seek"
    (let [a (sorted-iter (sorted-set :Esau :Isaac :Jacob :Joseph))
          b (sorted-iter (sorted-set :Hank :Isaac :Jacob :Levi))
          j (leapfrog-join [a b])]
      (is (= [:Isaac :Jacob] (vec j)))
      (is (= :Isaac (get-key (->seek j :Isaac))))
      (is (= :Jacob (get-key (->seek j :Jacob))))
      (is (nil? (->seek j :Zara))))))

(deftest trie-iterator-tests
  (let [rels (sorted-set [1 3 4] [1 3 5] [1 4 6] [1 4 8] [1 4 9] [1 5 2] [3 5 2])
        iter (trie-iterator rels)]
    (is (= 1 (-> iter get-key)))
    (is (= 3 (-> iter trie-open get-key)))
    (is (= 4 (-> iter trie-open trie-open get-key)))
    (is (= 5 (-> iter trie-open trie-open ->next get-key)))
    (is (= nil (-> iter trie-open trie-open ->next ->next)))
    (is (= nil (-> iter trie-open trie-open trie-open)))

    (testing "Metadata is passed through"
      (is (= {:a 1} (-> iter (with-meta {:a 1}) meta)))
      (is (= {:a 2} (-> iter (with-meta {:a 2}) trie-open meta)))
      (is (= {:a 3} (-> iter (with-meta {:a 3}) trie-open ->next meta))))

    (is (= 6 (-> iter trie-open ->next trie-open get-key)))))

(deftest trie-dfs-bfs-test
  (let [coll (sorted-set [1 2 3] [1 2 4] [1 6 7] [1 8 8] [2 5 5] [2 5 6])]
    (is (= [1 2 3 4 6 7 8 8 2 5 5 6]
           (dfs (trie-iterator coll))))
    (is (= [1 2 2 6 8 3 4 7 8 5 5 6]
           (bfs (trie-iterator coll))))))

(defn- test-trie-iter [variables tuples]
  {:variables variables :trie-iterator (trie-iterator tuples)})

(deftest trie-join*-smoke
  (let [relations [(test-trie-iter [:a] [[1] [2] [3] [4] [5]])
                   (test-trie-iter [:a] [[2] [4]])]]
    (is (some? (trie-join-iterator [:a] relations)))))

(def test-rel-1
  (test-trie-iter [:a :_x :b] [[1 "a" 10] [2 "b" 20] [3 "c" 30] [4 "d" 40] [5 "e" 50]]))

(def test-rel-2
  (test-trie-iter [:a :b] [[0 0] [1 10] [2 20] [5 50] [6 60]]))

(def odd-numbers   (test-trie-iter [:n] [[1] [3] [5] [7] [9] [11] [13] [15]]))
(def prime-numbers (test-trie-iter [:n] [[2] [3] [5] [7] [11] [13]]))
(def sq-numbers    (test-trie-iter [:n] [[1] [4] [9] [16]]))
(def nat-numbers   (test-trie-iter [:n] (map vector (range 1 17))))

(deftest trie-join*-1
  (let [join (trie-join-iterator [:a :_x :b] [test-rel-1 test-rel-2])]
    (is (= 1 (get-key join)))
    (is (= "a" (-> join trie-open get-key)))
    (is (= 10 (-> join trie-open trie-open get-key)))

    (is (= 0 (-> (trie-join [:a :b] [test-rel-2]) :trie-iterator get-key))))

  (testing "No intersection"
    (is (nil? (trie-join-iterator [:a :b]
                                  [(test-trie-iter [:a :b] [[1 10] [3 30]])
                                   (test-trie-iter [:a :b] [[2 20] [4 40]])])))))

(deftest trie-antijoin-tests
  (is (= [1 9 15] (vec (trie-antijoin-iterator odd-numbers prime-numbers))))

  (testing "Same variables"
    (let [a (test-trie-iter [:x :y] [[1 10] [1 11] [2 20] [2 21] [3 30] [4 44]])
          b (test-trie-iter [:x :y] [[0 1] [1 11] [2 20] [2 22] [4 40]])]
      (is (= [[1 10] [2 21] [3 30] [4 44]]
             (trie-routes (trie-antijoin-iterator a b))))))

  (testing "not-clause variables are a subset of clause variables"
    ;; not-clause has only [:x]; exclude all clause tuples where x matches
    (let [a (test-trie-iter [:x :y] [[1 10] [1 20] [2 10] [3 30]])
          b (test-trie-iter [:x]    [[1] [3]])]
      (is (= [[2 10]]
             (trie-routes (trie-antijoin-iterator a b))))))

  (testing "not-clause with no matching keys passes everything through"
    (let [a (test-trie-iter [:x :y] [[1 10] [2 20]])
          b (test-trie-iter [:x :y] [[5 50]])]
      (is (= [[1 10] [2 20]]
             (trie-routes (trie-antijoin-iterator a b))))))

  (testing "result inherits clause variables"
    (let [a (test-trie-iter [:p :q] [[1 2]])
          b (test-trie-iter [:p]    [[9]])]
      (is (= [:p :q] (:variables (trie-antijoin a b))))))

  (testing ":else branch — first clause variable absent from not-clause, lazy contract"
    (let [clause     (test-trie-iter [:a :b] [[1 10] [2 20] [3 30]])
          not-clause (test-trie-iter [:b]    [[10] [30]])
          iter       (trie-antijoin-iterator clause not-clause)]
      (is (= [1 2 3] (vec iter)))
      (is (nil? (trie-open iter)))
      (is (nil? (-> iter ->next ->next trie-open)))
      (is (= 20 (-> iter ->next trie-open get-key)))
      (is (= [[1] [2 20] [3]] (trie-routes iter)))))

  #_
  (testing "sync non-leaf match — lazy contract"
    (let [a    (test-trie-iter [:x :y] [[1 10] [1 11] [2 20] [5 99]])
          b    (test-trie-iter [:x :y] [[2 20] [5 99]])
          iter (trie-antijoin-iterator a b)]
      (is (= [1 2 5] (vec iter)))
      (is (= [10 11] (vec (trie-open iter))))
      (is (nil? (-> iter ->next trie-open)))
      (is (nil? (-> iter ->next ->next trie-open)))
      (is (= [[1 10] [1 11] [2] [5]] (trie-routes iter))))))

(deftest test-trie-routes
  (is (= [[1 2 3] [1 2 4] [1 6 7] [1 8 8] [2 5 5] [2 5 6]]
         (trie-routes (trie-iterator [[1 2 3] [1 2 4] [1 6 7] [1 8 8] [2 5 5] [2 5 6]])))))

(deftest union-tests
  (testing "Empty collection"
    (is (nil? (union-iterator []))))

  (testing "Single iterator"
    (is (= 1 (get-key (union-iterator [odd-numbers]))))
    (is (= [1 4 9 16] (vec (union-iterator [sq-numbers]))))
    (is (= [1 4 9 16] (vec (union-iterator [sq-numbers sq-numbers])))))

  (testing "Two iterators"
    (is (= [1 3 4 5 7 9 11 13 15 16]
           (vec (union-iterator [odd-numbers sq-numbers])))))

  (testing "->next advances to next key"
    (let [iter1 (test-trie-iter [:a] [[1] [2] [3] [4] [5]])
          iter2 (test-trie-iter [:a] [[2] [4] [6]])
          union-iter (union-iterator [iter1 iter2])]
      (is (= 1 (get-key union-iter)))
      (is (= 2 (-> union-iter ->next get-key)))
      (is (= 3 (-> union-iter ->next ->next get-key)))
      (is (= 4 (-> union-iter ->next ->next ->next get-key)))))

  (testing "->seek skips to given key"
    (let [iter (union-iterator [prime-numbers sq-numbers])]
      (are [k val] (= val (some-> iter (->seek k) get-key))
        1 1, 4 4, 6 7, 6.5 7, 20 nil)))

  (testing "Union over keyword-keyed tries"
    (let [iter1 (test-trie-iter [:a] [[:Isaac] [:Jacob]])
          iter2 (test-trie-iter [:a] [[:Esau] [:Jacob]])
          u     (union-iterator [iter1 iter2])]
      (is (= [:Esau :Isaac :Jacob] (vec u)))
      (is (= :Isaac (get-key (->seek u :Hank))))
      (is (= :Jacob (get-key (->seek u :Jacob))))
      (is (nil? (->seek u :Zara)))))

  #_(testing "All iterators exhausted"
      (let [iter1 (sorted-iter (sorted-set 1 2))
            iter2 (sorted-iter (sorted-set 1 2))
            union-iter (union [iter1 iter2])]
        (is (= 1 (get-key union-iter)))
        (is (= 2 (-> union-iter ->next get-key)))
        (is (nil? (-> union-iter ->next ->next)))))

  #_(testing "One iterator exhausts before others"
      (let [iter1 (sorted-iter (sorted-set 1 2))
            iter2 (sorted-iter (sorted-set 1 2 3 4))
            union-iter (union [iter1 iter2])]
        (is (= 1 (get-key union-iter)))
        (is (= 2 (-> union-iter ->next get-key)))
        (is (= 3 (-> union-iter ->next ->next get-key)))
        (is (= 4 (-> union-iter ->next ->next ->next get-key)))
        (is (nil? (-> union-iter ->next ->next ->next ->next)))))

  #_(testing "Many iterators with various sizes"
      (let [iters [(sorted-iter (sorted-set 1 5 9))
                   (sorted-iter (sorted-set 2 6 10))
                   (sorted-iter (sorted-set 3 7))
                   (sorted-iter (sorted-set 4 8))
                   (sorted-iter (sorted-set 1 2 3 4 5 6 7 8 9 10))]
            union-iter (union iters)]
        (is (= 1 (get-key union-iter)))
        (is (= [1 1 2 2 3 4 5 5 6 6 7 8 8 9 9 10 10] (vec union-iter)))))
  #_(testing "Iterator with single element"
      (let [iter1 (sorted-iter (sorted-set 5))
            iter2 (sorted-iter (sorted-set 5))
            union-iter (union [iter1 iter2])]
        (is (= 5 (get-key union-iter)))
        (is (= [5 5] (vec union-iter)))))

  #_(testing "->seek and ->next combined"
      (let [iter1 (sorted-iter (sorted-set 1 3 5 7 9 11))
            iter2 (sorted-iter (sorted-set 2 4 6 8 10))
            union-iter (union [iter1 iter2])]
        (is (= 1 (get-key union-iter)))
        ;; Seek to 4, should get 4
        (let [seeked (->seek union-iter 4)]
          (is (= 4 (get-key seeked)))
          ;; Next should be 5
          (is (= 5 (-> seeked ->next get-key)))
          ;; Seek from there to 8
          (is (= 8 (-> seeked ->next (->seek 8) get-key)))))))


(def exponents
  (test-trie-iter [:n1 :n2 :n3]
                  [[1 1 1]
                   [2 4 8]
                   [3 9 27]
                   [4 16 64]]))

(def get-tuples (comp trie-routes :trie-iterator))

(deftest eager-tests
  (testing "non-phantom relation is unchanged in tuple output"
    (let [rel (test-trie-iter [:x :y] [[1 10] [2 20]])]
      (is (= [[1 10] [2 20]]
             (trie-routes (:trie-iterator (eager {:variables [:x :y] :trie-iterator (:trie-iterator rel)})))))))

  (testing "antijoin :else phantoms are pruned"
    (let [aj (trie-antijoin (test-trie-iter [:a :b] [[1 10] [2 20] [3 30]])
                            (test-trie-iter [:b]    [[10] [30]]))]
      (is (= [[1] [2 20] [3]] (trie-routes (:trie-iterator aj))))
      (is (= [[2 20]]         (trie-routes (:trie-iterator (eager aj)))))
      (is (= [:a :b]          (:variables (eager aj))))))

  #_
  (testing "antijoin sync non-leaf phantoms are pruned"
    (let [aj (trie-antijoin (test-trie-iter [:x :y] [[1 10] [1 11] [2 20] [5 99]])
                            (test-trie-iter [:x :y] [[2 20] [5 99]]))]
      (is (= [[1 10] [1 11] [2] [5]] (trie-routes (:trie-iterator aj))))
      (is (= [[1 10] [1 11]]         (trie-routes (:trie-iterator (eager aj))))))))

(deftest test-reorder
  (testing "Trivial case, no reorder necessary"
    (is (= exponents (reorder [:n1 :n2 :n3] exponents))))

  (testing "Swap just 2 keys"
    (def data (test-trie-iter [:n1 :n2] [[1 1] [2 4] [3 9]]))
    (is (= [[1 1] [4 2] [9 3]]
           (get-tuples (reorder [:n2 :n1] data)))))

  (testing "Swap last two keys"
    (is (= [[1 1 1] [2 8 4] [3 27 9] [4 64 16]]
           (get-tuples (reorder [:n1 :n3 :n2] exponents)))))

  :OK)

(deftest relations-tests
  (testing "tuples become variable->value maps"
    (let [rel (test-trie-iter [:a :b] [[1 10] [2 20] [3 30]])]
      (is (= [{:a 1 :b 10} {:a 2 :b 20} {:a 3 :b 30}]
             (relations rel)))))

  (testing "single variable"
    (is (= [{:n 1} {:n 3} {:n 5}]
           (relations (test-trie-iter [:n] [[1] [3] [5]])))))

  (testing "three variables"
    (is (= [{:n1 1 :n2 1 :n3 1}
            {:n1 2 :n2 4 :n3 8}
            {:n1 3 :n2 9 :n3 27}
            {:n1 4 :n2 16 :n3 64}]
           (relations exponents))))

  (testing "phantom paths from antijoin are filtered out"
    (let [aj (trie-antijoin (test-trie-iter [:a :b] [[1 10] [2 20] [3 30]])
                            (test-trie-iter [:b]    [[10] [30]]))]
      (is (= [[1] [2 20] [3]] (trie-routes (:trie-iterator aj))))
      (is (= [{:a 2 :b 20}] (relations aj)))))

  (testing "empty relation yields empty sequence"
    (is (empty? (relations {:variables [:a :b] :trie-iterator nil})))))


(deftest filtering-tests
  (testing "predicate always true is a no-op over the relation"
    (is (= (trie-routes (:trie-iterator exponents))
           (trie-routes (:trie-iterator (filtering exponents [:n1] (constantly true))))))
    (is (= (trie-routes (:trie-iterator exponents))
           (trie-routes (:trie-iterator (filtering exponents [:n1 :n2] (constantly true)))))))

  (testing "predicate always false prunes all child subtrees from filter-var depth"
    (let [rel (test-trie-iter [:a :b] [[1 10] [2 20] [3 30]])
          out (filtering rel [:a] (constantly false))]
      (is (empty? (trie-routes (:trie-iterator out))))))

  (testing "filtering on prefix variable: keys past predicate-false branch have no children"
    (let [out (filtering exponents [:n1] odd?)]
      (is (= [[1 1 1] [3 9 27]]
             (trie-routes (:trie-iterator out))))))

  (testing "filtering on multiple variables uses bindings in filter-var order"
    (let [out (filtering exponents [:n1 :n2] <)]
      (is (= [[1] [2 4 8] [3 9 27] [4 16 64]]
             (trie-routes (:trie-iterator out))))))

  (testing "filter variables can be a non-prefix subseq of node variables"
    (let [out (filtering exponents [:n2] even?)]
      (is (= [[1] [2 4 8] [3] [4 16 64]]
             (trie-routes (:trie-iterator out))))))

  (testing "filtering composes with eager to drop phantom paths"
    (is (= [{:n1 2 :n2 4 :n3 8} {:n1 3 :n2 9 :n3 27} {:n1 4 :n2 16 :n3 64}]
           (relations (eager (filtering exponents [:n1 :n2] <))))))

  (testing "filtering-iterator returns the trie-iterator directly"
    (let [rel (test-trie-iter [:a :b] [[1 10] [2 20] [3 30] [4 40]])]
      (is (= [[2 20] [4 40]]
             (trie-routes (filtering-iterator rel [:a] even?)))))))


(deftest mapping-tests
  (testing "empty subseq prepends a constant level"
    (let [rel (test-trie-iter [:a :b] [[1 10] [2 20]])
          out (mapping rel [] (constantly 99) :c)]
      (is (= [:c :a :b] (:variables out)))
      (is (= [[99 1 10] [99 2 20]]
             (trie-routes (:trie-iterator out))))))

  (testing "single-arg mapping inserts level immediately after subseq variable"
    (let [rel (test-trie-iter [:n] [[1] [2] [3]])
          out (mapping rel [:n] #(* % %) :sq)]
      (is (= [:n :sq] (:variables out)))
      (is (= [[1 1] [2 4] [3 9]]
             (trie-routes (:trie-iterator out))))))

  (testing "multi-arg mapping passes bindings spread as args"
    (let [rel (test-trie-iter [:a :b] [[1 10] [2 20] [3 30]])
          out (mapping rel [:a :b] + :sum)]
      (is (= [:a :b :sum] (:variables out)))
      (is (= [[1 10 11] [2 20 22] [3 30 33]]
             (trie-routes (:trie-iterator out))))))

  (testing "non-prefix subseq inserts after the last subseq variable"
    (let [rel (test-trie-iter [:a :b :c] [[1 10 100] [2 20 200]])
          out (mapping rel [:a :c] + :sum)]
      (is (= [:a :b :c :sum] (:variables out)))
      (is (= [[1 10 100 101] [2 20 200 202]]
             (trie-routes (:trie-iterator out))))))

  (testing "insertion position depends on last subseq variable"
    (let [rel (test-trie-iter [:a :b :c] [[1 10 100]])]
      (is (= [:m :a :b :c]    (:variables (mapping rel []         (constantly 0) :m))))
      (is (= [:a :m :b :c]    (:variables (mapping rel [:a]       (constantly 0) :m))))
      (is (= [:a :b :m :c]    (:variables (mapping rel [:a :b]    (constantly 0) :m))))
      (is (= [:a :b :c :m]    (:variables (mapping rel [:a :b :c] (constantly 0) :m))))))

  (testing "->seek on inserted singleton level honors compare<= semantics"
    (let [rel  (test-trie-iter [:n] [[5]])
          out  (mapping rel [:n] #(* 2 %) :double)
          singleton (-> out :trie-iterator trie-open)]
      (is (= 10 (get-key singleton)))
      (is (= 10 (-> singleton (->seek 5) get-key)))
      (is (= 10 (-> singleton (->seek 10) get-key)))
      (is (nil? (->seek singleton 11)))
      (is (nil? (->next singleton)))))

  (testing "mapping composes with relations to populate the new variable"
    (let [rel (test-trie-iter [:a :b] [[1 10] [2 20]])
          out (mapping rel [:a :b] * :prod)]
      (is (= [{:a 1 :b 10 :prod 10}
              {:a 2 :b 20 :prod 40}]
             (relations out)))))

  (testing "mapping-iterator returns the trie-iterator directly"
    (let [rel (test-trie-iter [:n] [[1] [2] [3]])]
      (is (= [[1 1] [2 4] [3 9]]
             (trie-routes (mapping-iterator rel [:n] #(* % %) :sq)))))))


(deftest annotated-trie-iterator-tests
  (testing "Map input stores annotation at the leaf, accessible via Annotated"
    (let [iter (trie-iterator {[1 2 3] :foo})]
      (is (= 1 (get-key iter)))
      (is (= 2 (get-key (trie-open iter))))
      (is (= 3 (get-key (trie-open (trie-open iter)))))
      (is (= :foo (annotation (trie-open (trie-open iter)))))))

  (testing "Map input across multiple tuples"
    (let [iter (trie-iterator {[1 2] :a, [1 3] :b, [2 3] :c})]
      (is (= [:a :b :c]
             (for [k1 (iter-seq iter)
                   :let [child (-> iter (->seek k1) trie-open)]
                   k2 (iter-seq child)
                   :let [leaf (->seek child k2)]]
               (annotation leaf))))))

  (testing "Sequence input keeps the original behaviour (no annotation)"
    (let [iter (trie-iterator [[1 2 3]])]
      ;; The leaf reify still implements Annotation, but its value is nil
      ;; because no map values were supplied.
      (is (= nil (annotation (trie-open (trie-open iter))))))))

(deftest annotated-union-tests
  (testing "union with combine-fn sums annotations of coincident tuples"
    (let [r1 {:variables [:a :b]
              :trie-iterator (trie-iterator {[1 2] 3})}
          r2 {:variables [:a :b]
              :trie-iterator (trie-iterator {[1 2] 4})}
          u  (union [r1 r2] +)
          leaf (-> u :trie-iterator trie-open)]
      (is (= 7 (annotation leaf)))))

  (testing "union without combine-fn behaves like the original (set semantics)"
    (let [r1 {:variables [:a] :trie-iterator (trie-iterator [[1] [2]])}
          r2 {:variables [:a] :trie-iterator (trie-iterator [[2] [3]])}
          u  (union [r1 r2])]
      (is (= #{[1] [2] [3]}
             (set (trie-routes (:trie-iterator u))))))))


(deftest annotated-trie-join-tests
  (testing "trie-join with combine-fn multiplies annotations of contributing tuples"
    (let [r {:variables [:a :b] :trie-iterator (trie-iterator {[1 2] 3})}
          s {:variables [:b :c] :trie-iterator (trie-iterator {[2 9] 5})}
          j (trie-join [:a :b :c] [r s] *)
          leaf (-> j :trie-iterator trie-open trie-open)]
      (is (= 15 (annotation leaf)))))

  (testing "trie-join without combine-fn behaves like the original"
    (let [r {:variables [:a :b] :trie-iterator (trie-iterator [[1 2] [1 3]])}
          s {:variables [:b :c] :trie-iterator (trie-iterator [[2 9] [3 9]])}
          j (trie-join [:a :b :c] [r s])]
      (is (= #{[1 2 9] [1 3 9]}
             (set (trie-routes (:trie-iterator j))))))))


(deftest annotated-mapping-tests
  (testing "mapping inserted at the new leaf carries the underlying leaf's annotation"
    (let [rel  {:variables [:a :b] :trie-iterator (trie-iterator {[1 10] :alpha
                                                                  [2 20] :beta})}
          out  (mapping rel [:a :b] + :sum)
          leaf (-> out :trie-iterator trie-open trie-open)]
      (is (= [:a :b :sum] (:variables out)))
      (is (= 11 (get-key leaf)))
      (is (= :alpha (annotation leaf)))))

  (testing "mapping inserted before the original leaf preserves leaf annotations"
    (let [rel  {:variables [:a :b] :trie-iterator (trie-iterator {[1 10] :alpha
                                                                  [2 20] :beta})}
          out  (mapping rel [:a] #(* 2 %) :double)
          leaf (-> out :trie-iterator trie-open trie-open)]
      (is (= [:a :double :b] (:variables out)))
      (is (= 10 (get-key leaf)))
      (is (= :alpha (annotation leaf)))))

  (testing "mapping with empty subseq preserves leaf annotations"
    (let [rel  {:variables [:a :b] :trie-iterator (trie-iterator {[1 10] :alpha})}
          out  (mapping rel [] (constantly 99) :c)
          leaf (-> out :trie-iterator trie-open trie-open)]
      (is (= [:c :a :b] (:variables out)))
      (is (= :alpha (annotation leaf)))))

  (testing "mapping over a single-variable rel becomes a leaf-level singleton with annotation"
    (let [rel  {:variables [:n] :trie-iterator (trie-iterator {[5] :marker})}
          out  (mapping rel [:n] #(* % %) :sq)
          leaf (-> out :trie-iterator trie-open)]
      (is (= [:n :sq] (:variables out)))
      (is (= 25 (get-key leaf)))
      (is (= :marker (annotation leaf)))))

  (testing "mapping inserted in the middle preserves leaf annotation"
    (let [rel  {:variables [:a :b :c] :trie-iterator (trie-iterator {[1 2 3] :gamma})}
          out  (mapping rel [:a :b] + :m)
          leaf (-> out :trie-iterator trie-open trie-open trie-open)]
      (is (= [:a :b :m :c] (:variables out)))
      (is (= 3 (get-key leaf)))
      (is (= :gamma (annotation leaf))))))


(deftest annotated-reorder-tests
  (testing "no-op reorder preserves annotation"
    (let [rel  {:variables [:a :b] :trie-iterator (trie-iterator {[1 2] :alpha})}
          out  (reorder [:a :b] rel)
          leaf (-> out :trie-iterator trie-open)]
      (is (= [:a :b] (:variables out)))
      (is (= :alpha (annotation leaf)))))

  (testing "swapping two variables preserves the annotation of each tuple"
    (let [rel  {:variables [:a :b] :trie-iterator (trie-iterator {[1 10] :x
                                                                  [1 20] :y
                                                                  [2 10] :z})}
          out  (reorder [:b :a] rel)]
      (is (= [:b :a] (:variables out)))
      (is (= {[10 1] :x, [10 2] :z, [20 1] :y}
             (into {}
                   (for [b-iter (iterate ->next (:trie-iterator out))
                         :while b-iter
                         :let [a-iter (trie-open b-iter)]
                         leaf (iterate ->next a-iter)
                         :while leaf]
                     [[(get-key b-iter) (get-key leaf)] (annotation leaf)]))))))

  (testing "reorder swapping last two of three variables preserves leaf annotations"
    (let [rel  {:variables [:a :b :c] :trie-iterator (trie-iterator {[1 2 3] :p
                                                                     [1 4 5] :q})}
          out  (reorder [:a :c :b] rel)]
      (is (= [:a :c :b] (:variables out)))
      (is (= {[1 3 2] :p, [1 5 4] :q}
             (into {}
                   (for [a-iter (iterate ->next (:trie-iterator out))
                         :while a-iter
                         :let [c-iter (trie-open a-iter)]
                         c-iter (iterate ->next c-iter)
                         :while c-iter
                         :let [b-iter (trie-open c-iter)]
                         leaf (iterate ->next b-iter)
                         :while leaf]
                     [[(get-key a-iter) (get-key c-iter) (get-key leaf)]
                      (annotation leaf)])))))))


(defn- annotated-tuples
  "Walks rel's trie and returns a {path → leaf-annotation} map."
  [{:keys [trie-iterator]}]
  (letfn [(walk [iter prefix]
            (mapcat (fn [it]
                      (let [path (conj prefix (get-key it))]
                        (if-let [child (trie-open it)]
                          (walk child path)
                          [[path (annotation it)]])))
                    (take-while some? (iterate ->next iter))))]
    (into {} (walk trie-iterator []))))

(deftest omit-tests
  (testing "identity omit preserves all tuples"
    (let [out (omit (test-trie-iter [:a :b] [[1 10] [2 20]]) [:a :b] +)]
      (is (= [:a :b] (:variables out)))
      (is (= {[1 10] nil, [2 20] nil} (annotated-tuples out)))))

  (testing "omitting the trailing variable combines merged annotations"
    (let [out (omit (test-trie-iter [:a :b] {[1 10] 1, [1 20] 2, [2 30] 4}) [:a] +)]
      (is (= [:a] (:variables out)))
      (is (= {[1] 3, [2] 4} (annotated-tuples out)))))

  (testing "omitting the leading variable combines across collapsed paths"
    (let [out (omit (test-trie-iter [:a :b] {[1 10] 5, [2 10] 7, [3 20] 9}) [:b] +)]
      (is (= [:b] (:variables out)))
      (is (= {[10] 12, [20] 9} (annotated-tuples out)))))

  (testing "omitting a middle variable combines across the omitted layer"
    (let [out (omit (test-trie-iter [:a :b :c] {[1 5 10] 100, [1 6 10] 200, [1 6 20] 50}) [:a :c] +)]
      (is (= [:a :c] (:variables out)))
      (is (= {[1 10] 300, [1 20] 50} (annotated-tuples out)))))

  (testing "combine-fn is not invoked when projections are unique"
    (let [calls (atom 0)
          cb    (fn [a b] (swap! calls inc) (+ a b))]
      (annotated-tuples (omit (test-trie-iter [:a :b] {[1 10] 1, [2 20] 2}) [:a] cb))
      (is (= 0 @calls))))

  (testing "phantom paths from antijoin are not promoted to complete tuples"
    (let [base (test-trie-iter [:a :b :c] [[1 5 10] [2 5 10] [2 6 20] [3 7 30]])
          rel  (trie-antijoin base (test-trie-iter [:c] [[10]]))]
      (is (= [[1 5] [2 5] [2 6 20] [3 7 30]]
             (trie-routes (:trie-iterator rel)))
          "sanity: rel has phantom paths [1 5] and [2 5]")
      (is (= [{:b 6 :c 20} {:b 7 :c 30}]
             (relations (omit rel [:b :c] +))))
      (is (= [{:a 2 :c 20} {:a 3 :c 30}]
             (relations (omit rel [:a :c] +))))
      (is (= [{:a 2 :b 6}  {:a 3 :b 7}]
             (relations (omit rel [:a :b] +)))))))

