(ns clojurewerkz.statistiker.summary-test
  (:require [clojure.test :refer :all]
            [clojurewerkz.statistiker.fast-math :refer :all]
            [clojurewerkz.statistiker.summary :refer :all]))

(deftest mean-test
  (is (= 5.5 (mean [1 2 3 4 5 6 7 8 9 10]))))

(deftest standard-deviation-test
  (is (equals 3.02 (standard-deviation [1 2 3 4 5 6 7 8 9 10]) 0.01)))

(deftest variance-test
  (is (equals 9.16 (variance [1 2 3 4 5 6 7 8 9 10]) 0.01)))
