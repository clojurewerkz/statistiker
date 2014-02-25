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


(def test-item [6	130	8])

(def test-data
  {:male   [[5.92, 190, 11]
            [5.58, 170, 12]
            [5.92, 165, 10]
            [6, 180,12]
            [7,220, 11]]
   :female [[5, 100, 6]
            [5.5, 150, 8]
            [5.42, 130, 7]
            [5.75, 150, 9]]})

(def test-data2 {:male	[[6	180	12]
                         [5.92	190	11]
                         [5.58 170	12]
                         [5.92 165	10]]

                 :female	[ [5	100	6]
                            [5.5	150	8]
                            [5.42	130	7]
                            [5.75	150	9]]})
