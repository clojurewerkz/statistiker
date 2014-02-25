(ns clojurewerkz.statistiker.descriptive
  (:require [clojurewerkz.statistiker.fast-math :refer :all]))

(defn get-rank
  [amount percentile]
  (assert (<= percentile 100))
  (-> (* (/ percentile 100) (dec amount))
      Math/ceil
      int))

(def percentiles
  {:min 0
   :max 100
   :median 50
   :25 25
   :75 75})

(defn fivenum
  [values]
  (let [sorted (sort values)
        amount (count values)]
    {:min    (nth sorted (get-rank amount 0))
     :25     (nth sorted (get-rank amount 25))
     :median (nth sorted (get-rank amount 50))
     :75     (nth sorted (get-rank amount 75))
     :max    (nth sorted (get-rank amount 100) )}))
