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


(def classification-data-maps
  [{:label :male :height 6.0 :weight	180 :foot-size	12}
   {:label :male :height 5.92 :weight	190 :foot-size	11}
   {:label :male :height 5.58 :weight 170 :foot-size	12}
   {:label :male :height 5.92 :weight 165 :foot-size	10}
   {:label :female :height 5.0 :weight	100 :foot-size	6}
   {:label :female :height 5.5 :weight	150 :foot-size	8}
   {:label :female :height 5.42 :weight	130 :foot-size	7}
   {:label :female :height 5.75 :weight	150 :foot-size	9}])


(deftest test-classify-maps
  (testing "eucledian distance"
    (let [model      (maps->model classification-data-maps :label [:height :weight :foot-size])
          classified (classify model test-item 5)]
      (is (= :female (best-match classified)))))
  (testing "cranberra distance"
    (let [model      (maps->model classification-data-maps :label [:height :weight :foot-size])
          classified (classify model test-item 5 distance/canberra-distance)]
      (is (= :female (best-match classified)))))
  (testing "chebyshev distance"
    (let [model      (maps->model classification-data-maps :label [:height :weight :foot-size])
          classified (classify model test-item 5 distance/chebyshev-distance)]
      (is (= :female (best-match classified)))))
  (testing "manhattan distance"
    (let [model      (maps->model classification-data-maps :label [:height :weight :foot-size])
          classified (classify model test-item 5 distance/manhattan-distance)]
      (is (= :female (best-match classified))))))
