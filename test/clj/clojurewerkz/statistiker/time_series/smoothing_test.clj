(ns clojurewerkz.statistiker.time-series.smoothing-test
  (:require [clojure.test :refer :all]
            [clojurewerkz.statistiker.time-series.smoothing :refer :all]))

(deftest fixed-size-conj-test
  (let [f (fixed-size-conj 5)]
    (is (= [2 3 4 5 6] (f [1 2 3 4 5] 6)))
    (is (= [1 2 3 4] (f [1 2 3] 4)))
    (is (= [4 5 6 7 8] (f [1 2 3 4 5 6 7] 8)))))

(deftest linear-smooth-seq-test
  (is (= [3.0 4.0 5.0 6.0] (linear-smooth-seq 5 [1 2 3 4 5 6 7 8]))))

(deftest linear-smooth-stream-test
  (let [res (atom [])
        f   (linear-smooth-stream 5 (fn [a] (swap! res conj a)))]

    (doseq [i [1 2 3 4 5 6 7 8]]
      (f i))

    (is (= [3.0 4.0 5.0 6.0] @res)))
  (let [res (atom [])
        f   (linear-smooth-stream 5 (fn [a] (swap! res conj a)) :val)]

    (doseq [i [{:val 1 :meta 1}
               {:val 2 :meta 2}
               {:val 3 :meta 3}
               {:val 4 :meta 4}
               {:val 5 :meta 5}
               {:val 6 :meta 6}
               {:val 7 :meta 7}
               {:val 8 :meta 8}]]
      (f i))

    (is (= [{:val 3.0 :meta 5}
            {:val 4.0 :meta 6}
            {:val 5.0 :meta 7}
            {:val 6.0 :meta 8}]
           @res))))
