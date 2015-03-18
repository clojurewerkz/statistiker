(ns clojurewerkz.statistiker.metrics-test
	(:require [clojurewerkz.statistiker.metrics :refer :all]            
            [clojure.test :refer :all]))


(deftest wrong-input-ami
	(is (thrown? AssertionError (adjusted_mutual_information [1 2 3] [2 3])))
	(is (thrown? AssertionError (adjusted_mutual_information [] [2 3])))
	(is (thrown? AssertionError (adjusted_mutual_information [:b :a :c] [2 :a]))))

(deftest wrong-input-mi
	(is (thrown? AssertionError (mutual_information [1 2 3] [2 3])))
	(is (thrown? AssertionError (mutual_information [] [2 3])))
	(is (thrown? AssertionError (mutual_information [:b :a :c] [2 :a])))
	(is (thrown? AssertionError (mutual_information [] []))))

(deftest adjusted_mutual_information-values
	(is (= (adjusted_mutual_information [1 2 3] [1 2 3]) 
			(adjusted_mutual_information [3 2 1] [1 2 3])
			(adjusted_mutual_information [1 2 3] [:a :b 2])
			1.0))
	(is (= (adjusted_mutual_information [] []) 1.0))
	(is (= (adjusted_mutual_information [1 2 3 4] [1 1 1 1]) 0.0))
	(is (= (adjusted_mutual_information [:a :a :a :a] [1 1 1 1]) 1.0))
	(is (= (adjusted_mutual_information [:a 6 :d :f] [1 2 3 4]) 1.0))
	(is (= (adjusted_mutual_information [0, 0, :c2, :c1], [0, 0, 87, 99]) 1.0))
	(is (= (adjusted_mutual_information [0, 0, 'docid2', 'docid'], [0, 99, 87, 0]) -0.20000000857324676))
	(is (= (adjusted_mutual_information [0, 1, 2, 0], [0, 1, 2, 3]) -8.2435062679677756e-09)))


(deftest mutual_information-values
	(is (= (mutual_information [1 2 3] [1 2 3]) 
			(mutual_information [3 2 1] [1 2 3])
			(mutual_information [1 2 3] [:a :b 2])
			1.0986122886681096))
	(is (= (mutual_information [1 2 3 4] [1 1 1 1]) 0.0))
	(is (= (mutual_information [:a :a :a :a] [1 1 1 1]) 0.0))
	(is (= (mutual_information [:a 6 :d :f] [1 2 3 4]) 0.0))
	(is (= (mutual_information [0, 0, :c2, :c1], [0, 0, 87, 99]) 1.0397207708399179))
	(is (= (mutual_information [0, 0, 'docid2', 'docid'], [0, 99, 87, 0]) 0.69314718055994518))
	(is (= (mutual_information [0, 1, 2, 0], [0, 1, 2, 3]) 1.0397207708399179)))