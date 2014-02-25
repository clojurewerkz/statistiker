(ns clojurewerkz.statistiker.classification.naive-bayes
  (:require [clojure.test :refer :all]
            [clojurewerkz.statistiker.classification.naive-bayes :refer :all]))


(def test-item [6	130	8])

(def classification-data {:male	  [[6	180	12]
                                   [5.92	190	11]
                                   [5.58 170	12]
                                   [5.92 165	10]]
                          :female [[5	100	6]
                                   [5.5	150	8]
                                   [5.42	130	7]
                                   [5.75	150	9]]})

(deftest test-classify
  (let [model      (make-model classification-data)
        classified (classify model test-item)
        best-match (best-match classified)]
    (println (> (second (vals classified)) (first (vals classified))) )
    (is (= :female (first best-match)))))
