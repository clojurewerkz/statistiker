(ns clojurewerkz.statistiker.descriptive
  (:import [org.apache.commons.math3.stat.descriptive.rank Percentile])
  (:require [clojurewerkz.statistiker.fast-math :refer :all]))

(def percentiles
  {:min 1
   :max 100
   :median 50
   :25 25
   :75 75})

(defn fivenum
  [values]
  (let [p (Percentile.)]
    (.setData p (double-array values))
    (reduce (fn [acc [k v]]
              (assoc acc k (.evaluate p (double v))))
            {}
            percentiles)))

(defn iqr
  [values]
  (let [p              (Percentile.)
        _              (.setData p (double-array values))
        first-quartile (.evaluate p (double 25))
        third-quartile (.evaluate p (double 75))]
    (- third-quartile first-quartile)))
