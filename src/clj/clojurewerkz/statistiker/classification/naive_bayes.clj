(ns clojurewerkz.statistiker.classification.naive-bayes
  (:require [clojurewerkz.statistiker.fast-math  :refer :all]
            [clojure.core.matrix                 :refer [transpose]]
            [clojurewerkz.statistiker.utils      :refer :all]
            [clojurewerkz.statistiker.statistics :refer :all]))

(def pi Math/PI)

(defn make-model
  [train-data]
  (let [total (reduce + (map count (vals train-data)))]
    (into {}
          (for [[k v] train-data]
            [k {:p                   (/ (count v) total)
                ;; ? Evidence
                :classification-data (->> v
                                          transpose
                                          (mapv (fn [v]
                                                  (let [var (variance v)]
                                                    (assert (not (= 0.0 var))
                                                            (str "Variance of " k " is " var))
                                                    {:mean (mean v)
                                                     :variance var}))))}]))))

(defn maps->model
  [maps label features]
  (->> maps
      vec
      (group-by #(get % label))
      (map-groups (fn [items] (mapv #(select-keys-order-dependent % features) items)))
      make-model))

(defn posterior-prob
  "Calculate probability for the single item"
  [point variance mean]
  (* (/ 1 (sqrt (* 2 pi variance)))
     (exp (/ (* -1 (pow (- point mean) 2))
             (* 2 variance)))))

(defn classify
  [model item]
  (into {}
        (for [[k {:keys [classification-data p]}] model]
          [k
           (->> classification-data
                (mapv (fn [point {:keys [mean variance]}]
                        (posterior-prob point variance mean))
                      item)
                ;; TODO: add probability and confidence
                (reduce * p))])))

(defn best-match
  [posterior-probabilities]
  (loop [[[k v] & tail] (vec posterior-probabilities)
         max            Integer/MIN_VALUE
         max-key        nil]
    (let [[max max-key] (if (> max v)
                          [max max-key]
                          [v k])]
      (if (empty? tail)
        [max-key max]
        (recur tail v k)))))
