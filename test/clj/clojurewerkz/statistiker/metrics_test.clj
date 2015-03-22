(ns clojurewerkz.statistiker.metrics-test
  (:require [clojurewerkz.statistiker.metrics :refer :all]
            [clojurewerkz.statistiker.utils :refer [almost=]]
            [clojure.test :refer :all]))
; Tolerance for values
(def tol 1e-8)

(deftest wrong-input-mi
  (testing "invalid inputs for mutual-information"
    (are [U V] (thrown? AssertionError (mutual-information U V))
      [1 2 3]       [2 3]
      []            [2 3]
      [:b :a :c]    [2 :a]
      []            []
      [[:b :a] :c]  [2 :a])))

(deftest wrong-input-ami
  (testing "invalid inputs for adjusted-mutual-information"
    (are [U V] (thrown? AssertionError (adjusted-mutual-information U V)) 
      [1 2 3]       [2 3]
      []            [2 3]
      [:b :a :c]    [2 :a]
      [[:b :a] :c]  [2 :a])))

(deftest mutual-information-values
  (testing "mutual information calculations"
    (are [U V s] (almost= (mutual-information U V) s tol)
      [3 2 1]                 [1 2 3]     1.0986122886681096 
      [1 2 3]                 [1 2 3]     1.0986122886681096
      [1 1 1]                 [1 1 1]     0.0
      [1 2 3 4]               [1 1 1 1]   0.0
      [:a :a :a :a]           [1 1 1 1]   0.0
      [:a 6 :d :f]            [1 2 3 4]   1.3862943611198906
      [0 0 :c2 :c1]           [0 0 87 99] 1.0397207708399179
      [0 0 'docid2' 'docid']  [0 99 87 0] 0.6931471805599453
      [0 1 2 0]               [0 1 2 3]   1.0397207708399179)))

(deftest adjusted-mutual-information-values
  (testing "adjusted mututal information calculations"
    (are [U V s] (almost= (adjusted-mutual-information U V) s tol)
      [1 2 3]                 [1 2 3]     1.0
      [3 2 1]                 [1 2 3]     1.0
      [1 2 3]                 [:a :b 2]   1.0
      []                      []          1.0
      [1 2 3 4]               [1 1 1 1]   0.0
      [:a :a :a :a]           [1 1 1 1]   1.0   ;special limit case
      [:a 6 :d :f]            [1 2 3 4]   1.0
      [0 0 :c2 :c1]           [0 0 87 99] 1.0
      [0 0 'docid2' 'docid']  [0 99 87 0] -0.20000000000000023
      [0 1 2 0]               [0 1 2 3]   0.0)))
