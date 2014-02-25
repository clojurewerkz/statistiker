(ns clojurewerkz.statistiker.classification.naive-bayes
  (:require [clojure.core.matrix :refer [transpose]])
  (:require [clojurewerkz.statistiker.fast-math :refer :all])
  (:require [clojurewerkz.statistiker.summary :refer :all]))

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
                                                  {:mean (mean v)
                                                   :variance (variance v)})))}]))))

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
