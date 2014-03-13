(ns clojurewerkz.statistiker.entropy-test
  (:require [clojurewerkz.statistiker.entropy :refer :all]
            [clojure.test :refer :all]))

(deftest shannon-entropy-test
  (is (= (shannon-entropy [1,1,5,1,1])
         (shannon-entropy [1,1,5,1,1])))

  (is (> (shannon-entropy [1,1,1,1,1])
         (shannon-entropy [1,1,5,1,1]))))
