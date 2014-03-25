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

(defn find-cluster
  [clusters item-pred]
  (->> clusters
       (filter (fn [points] (some item-pred points)))
       first
       set))

(deftest cluster-by-test
  (let [c        (cluster-by [{:a 1 :b 1 :c 1}
                              {:a 2 :b 2 :c 2}
                              {:a 3 :b 3 :c 3}
                              {:a 50 :b 50 :c 50}
                              {:a 51 :b 51 :c 51}
                              {:a 53 :b 53 :c 53}
                              {:a 54 :b 54 :c 54}]
                             [:a :b :c]
                             2
                             100)
        clusters (mapv second (vec (group-by :cluster-id c)))]
    (let [cluster-with-1 (find-cluster clusters #(= 1 (:a %)))]
      (= 3 (count cluster-with-1))
      (is (some #(= 2 (:a %)) cluster-with-1))
      (is (some #(= 3 (:a %)) cluster-with-1)))

    (let [cluster-with-50 (find-cluster clusters #(= 50 (:a %)))]
      (= 4 (count cluster-with-50))
      (is (some #(= 51 (:a %)) cluster-with-50))
      (is (some #(= 53 (:a %)) cluster-with-50))
      (is (some #(= 54 (:a %)) cluster-with-50)))))
