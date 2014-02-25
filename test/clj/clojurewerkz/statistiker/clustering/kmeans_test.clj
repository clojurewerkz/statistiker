(ns clojurewerkz.statistiker.clustering.kmeans-test
  (:require [clojure.test :refer :all]
            [clojurewerkz.statistiker.clustering.kmeans :refer :all]))

(deftest cluster-test
  (let [c (cluster [(with-meta [1 1 1] {:a 1}) [2 2 2] [3 3 3]
                    [50 50 50] [51 51 51] [53 53 53]]
                   2
                   100)
        c (sort-by #(get-in % [:center 0]) c)]
    (is (= 1.0 (-> c first :points ffirst)))
    (is (= {:a 1} (-> c first :points first meta)))
    (is (= 50.0 (-> c second :points ffirst)))))
