(ns clojurewerkz.statistiker.classification.k-nearest-neighbours-test
  (:require [clojurewerkz.statistiker.classification.k-nearest-neighbours :refer :all]
            [clojurewerkz.statistiker.distance :as distance]
            [clojure.test :refer :all]))

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
  (testing "eucledian distance"
    (let [model      (make-model classification-data)
          classified (classify model test-item 5)]
      (is (= :female (best-match classified)))))
  (testing "cranberra distance"
    (let [model      (make-model classification-data)
          classified (classify model test-item 5 distance/canberra-distance)]
      (is (= :female (best-match classified)))))
  (testing "chebyshev distance"
    (let [model      (make-model classification-data)
          classified (classify model test-item 5 distance/chebyshev-distance)]
      (is (= :female (best-match classified)))))
  (testing "manhattan distance"
    (let [model      (make-model classification-data)
          classified (classify model test-item 5 distance/manhattan-distance)]
      (is (= :female (best-match classified))))))
