(ns clojurewerkz.statistiker.fitting-test
  (:require [clojurewerkz.statistiker.fitting :refer :all]
            [clojure.test :refer :all]))

(deftest gaussian-fitting-test
  (comment
    (let [[a b c] (gaussian-fitting [[2.8 -48.8] [4.8 50.5] [4.6 42.0] [2.65 -45.5]
                                     [3.8 -13.5] [3.9 -7.0] [4.9 54.0] [5.65 36.0]
                                     [2.96 -54.0] [5.92 14.5]])
          g       (gaussian-function a b c)]

      )))
