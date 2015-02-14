(ns clojurewerkz.statistiker.scaling-test
  (:require [clojurewerkz.statistiker.scaling :refer :all]
            [clojure.test :refer :all]))


(deftest rescale-test
  (is (= [0.0 0.25 0.5 0.75 1.0] (rescale [2 4 6 8 10])))
  (is (= [0.0 0.25 0.5 0.75 1.0] (rescale [-4 -2 0 2 4]))))

(deftest rescale-range-test
  (is (= [-0.5 -0.25 0.0 0.25 0.5] (rescale-range [-4 -2 0 2 4] -0.5 0.5)))
  (is (= [-4.0 -2.0 0.0 2.0 4.0] (rescale-range [-4 -2 0 2 4] -4 4))))

(deftest standartise-test
  (is (= [(/ -25 (Math/sqrt 1250))
          (/ 25  (Math/sqrt 1250))]
         (standartise [50 100]))))

(deftest lp-norm-wrong-param
   (is (thrown? AssertionError (make-lp-normalize-fn -2 [1 2 3])))
   (is (thrown? AssertionError (make-lp-normalize-fn 0 [1 2 3])))
   (is (thrown? AssertionError (make-lp-normalize-fn 3.7 [1 2 3]))))

(deftest l1-normalize-test
  (is (= (map double [(/ 2 10)
                      (/ 2 10)
                      (/ 6 10)])
         (l1-normalize [2 2 6]))))


(deftest l2-normalize-test
  (is (= (map double [(/ 10 (Math/sqrt 125))
                      (/ 5 (Math/sqrt 125))])
         (l2-normalize [10 5]))))


(deftest scale-feature-test
  (is (= [{:a 0.0 :b 1}
          {:a 0.25 :b 2}
          {:a 0.5 :b 4}
          {:a 0.75 :b 4}
          {:a 1.0  :b 5}]
         (scale-feature [{:a 2 :b 1}
                         {:a 4 :b 2}
                         {:a 6 :b 4}
                         {:a 8 :b 4}
                         {:a 10  :b 5}]
                        :a
                        make-rescale-fn
                        ))))
