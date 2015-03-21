(ns clojurewerkz.statistiker.metrics-test
  (:require [clojurewerkz.statistiker.metrics :refer :all]
            [clojurewerkz.statistiker.utils :refer [almost=]]
            [clojure.test :refer :all]))
; Tolerance for values
(def tol 1e-8)

(deftest wrong-input-mi
  (testing "invalid inputs for mutual-information"
    (is (thrown? AssertionError (mutual-information [1 2 3] [2 3])))
    (is (thrown? AssertionError (mutual-information [] [2 3])))
    (is (thrown? AssertionError (mutual-information [:b :a :c] [2 :a])))
    (is (thrown? AssertionError (mutual-information [] [])))
    (is (thrown? AssertionError (adjusted_mutual-information [[:b :a] :c] [2 :a])))))

(deftest wrong-input-ami
  (testing "invalid inputs for adjusted_mutual-information"
    (is (thrown? AssertionError (adjusted_mutual-information [1 2 3] [2 3])))
    (is (thrown? AssertionError (adjusted_mutual-information [] [2 3])))
    (is (thrown? AssertionError (adjusted_mutual-information [:b :a :c] [2 :a])))
    (is (thrown? AssertionError (adjusted_mutual-information [[:b :a] :c] [2 :a])))))

(deftest mutual-information-values
  (testing "mutual information calculations"
    (is (= (mutual-information [1 2 3] [1 2 3])
           (mutual-information [3 2 1] [1 2 3])
           (mutual-information [1 2 3] [:a :b 2])))
    (is (almost= (mutual-information [1 2 3] [1 2 3]) 1.0986122886681096 tol))
    (is (= (mutual-information [1 1 1] [1 1 1]) 0.0))
    (is (= (mutual-information [1 2 3 4] [1 1 1 1]) 0.0))
    (is (= (mutual-information [:a :a :a :a] [1 1 1 1]) 0.0))
    (is (almost= (mutual-information [:a 6 :d :f] [1 2 3 4]) 1.3862943611198906 tol))
    (is (almost= (mutual-information [0 0 :c2 :c1] [0 0 87 99]) 1.0397207708399179 tol))
    (is (almost= (mutual-information [0 0 'docid2' 'docid'] [0 99 87 0]) 0.6931471805599453 tol))
    (is (almost= (mutual-information [0 1 2 0] [0 1 2 3]) 1.0397207708399179 tol))))

(deftest adjusted-mutual-information-values
  (testing "adjusted mututal information calculations"
    (is (= (adjusted-mutual-information [1 2 3] [1 2 3])
           (adjusted-mutual-information [3 2 1] [1 2 3])
           (adjusted-mutual-information [1 2 3] [:a :b 2])
           1.0))
  	(is (= (adjusted-mutual-information [] []) 1.0))
    (is (==(adjusted-mutual-information [1 2 3 4] [1 1 1 1]) 0.0))
    (is (= (adjusted-mutual-information [:a :a :a :a] [1 1 1 1]) 1.0))   ;special limit case
    (is (= (adjusted-mutual-information [:a 6 :d :f] [1 2 3 4]) 1.0))
    (is (= (adjusted-mutual-information [0 0 :c2 :c1] [0 0 87 99]) 1.0))
    (is (almost= (adjusted-mutual-information [0 0 'docid2' 'docid'] [0 99 87 0]) -0.20000000000000023 tol))
    (is (= (adjusted-mutual-information [0 1 2 0] [0 1 2 3]) 0.0))))
