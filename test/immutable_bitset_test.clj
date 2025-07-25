(ns immutable-bitset-test
  (:use
    [clojure test]
    [immutable-bitset])
  (:require
;    [criterium.core :as c]
    [clojure.set :as s]
    [collection-check.core :as check]
    [clojure.test.check.generators :as gen])
;  (:import
;    [immutable_bitset BitSet])
    )

(deftest test-equivalency
  (check/assert-set-like 1e3 (sparse-bitset) gen/pos-int)
  (check/assert-set-like 1e3 (dense-bitset) gen/pos-int))

(defn run-test-set-algebra [constructor union intersection difference]
  (let [a (constructor (range 11))
        b (constructor (range 10 20))]
    (is (= (set (range 20))
          (union a b)
           (union b a)))
    (is (= 20
          (count (union b a))
          (count (union a b))))
    (is (= #{10}
          (intersection a b)
          (intersection b a)))
    (is (= 1
          (count (intersection b a))
          (count (intersection a b))))
    (is (= (set (range 10)) (difference a b)))
    (is (= (set (range 11 20)) (difference b a)))
    (is (= 10 (count (difference a b))))
    (is (= 9 (count (difference b a)))))

  (let [s-a (range 10)
        s-b (range 10000 10010)
        a (constructor s-a)
        b (constructor s-b)]
    (is (= (set (concat s-a s-b))
          (union a b)
          (union b a)))
    (is (= 20
          (count (union b a))
          (count (union a b))))
    (is (= #{}
          (intersection a b)
          (intersection b a)))
    (is (= 0
          (count (intersection b a))
          (count (intersection b a))))
    (is (= (set s-a) (difference a b)))
    (is (= (set s-b) (difference b a)))
    (is (= 10 (count (difference a b))))
    (is (= 10 (count (difference b a))))))

(deftest test-set-algebra
  (run-test-set-algebra set s/union s/intersection s/difference)
  (run-test-set-algebra sparse-bitset union intersection difference)
  (run-test-set-algebra dense-bitset union intersection difference))

(deftest hash-consistency
  (doseq [elems [ (range 10) [] ]]
    (let [s1a (set elems)
          s1b (sparse-bitset elems)
          s1c (dense-bitset elems)]
    (is (= s1a s1b s1c))
    (is (= (hash s1a) (hash s1b) (hash s1c))))))

;;;
;
;(deftest ^:benchmark benchmark-modify-set
;  (println "sparse bitset into 1e3")
;  (c/quick-bench
;    (into (sparse-bitset) (range 1e3)))
;  (println "dense bitset into 1e3")
;    (c/quick-bench
;    (into (dense-bitset) (range 1e3)))
;  (println "normal set into 1e3")
;  (c/quick-bench
;    (into #{} (range 1e3)))
;  (println "mutable bitset add 1e3")
;  (c/quick-bench
;    (let [^BitSet bitset (BitSet. 1e3)]
;      (dotimes [idx 1e3]
;        (.set bitset idx true)))))
;
;(deftest ^:benchmark benchmark-check-set
;  (println "check sparse bitset")
;  (let [b (into (sparse-bitset) (range 1e3))]
;    (c/quick-bench
;      (contains? b 123)))
;  (println "check dense bitset")
;  (let [b (into (dense-bitset) (range 1e3))]
;    (c/quick-bench
;      (contains? b 123)))
;  (println "check normal set")
;  (let [s (into #{} (range 1e3))]
;    (c/quick-bench
;      (contains? s 123)))
;  (println "mutable bitset lookup")
;  (let [b (BitSet. 1e3)]
;    (c/quick-bench
;      (.get b 123))))
