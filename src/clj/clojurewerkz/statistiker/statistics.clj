(ns clojurewerkz.statistiker.statistics
  "Basic statistics"
  (:import [org.apache.commons.math3.stat.descriptive.rank Percentile]
           [org.apache.commons.math3.stat StatUtils])
  (:require [clojurewerkz.statistiker.fast-math :refer [sqrt pow]]))

(defn mean
  "Computes the ariphmetic mean of values."
  [values]
  (let [c (count values)]
    (if (> c 0)
      (float (/ (reduce + values) (count values)))
      0)))

(defn variance
  "Computes a sample variance of values"
  [values]
  (StatUtils/variance (double-array values)))

(defn standard-deviation
  "Computes Standard Deviation of values (square root of variance)"
  [values]
  (sqrt (variance values)))

(def sd standard-deviation)

(defn geometric-mean
  "Computes a [geometric mean](http://www.xycoon.com/geometric_mean.htm) of values"
  [values]
  (StatUtils/geometricMean (double-array values)))

(defn mode
  "Computes a mode (the value that appears most often within the data)"
  [values]
  (vec (StatUtils/mode (double-array values))))

(defn product
  "Computes a product of values"
  [values]
  (reduce * values))

(def ^:private percentile-mappings
  {:min    1
   :max    100
   :median 50
   :25     25
   :75     75})

(defn fivenum
  "Computes 5-number statistics for given values (min, 25 percentile, median, 75 percentile and max)"
  [values]
  (let [p (Percentile.)]
    (.setData p (double-array values))
    (reduce (fn [acc [k v]]
              (assoc acc k (.evaluate p (double v))))
            {}
            percentile-mappings)))

(defn iqr
  "Computes an interquartile range of values"
  [values]
  (let [p              (Percentile.)
        _              (.setData p (double-array values))
        first-quartile (.evaluate p (double 25))
        third-quartile (.evaluate p (double 75))]
    (- third-quartile first-quartile)))

(defn median
  "Computes median of data"
  [values]
  (let [p (Percentile.)
        _ (.setData p (double-array values))]
    (.evaluate p (double 50))))

(defn percentiles
  "Computes percentiles for values, for example:

      (percentiles [1 2 3 4 5 6 7 8 9] [0 25 50 75 100])

   Would return 5-number values"
  [values percentiles]
  (let [p (Percentile.)
        _ (.setData p (double-array values))]
    (mapv #(.evaluate p (double %)) percentiles)))

(defn percentile
  "Computes a single percentile for given data"
  [values percentile]
  (let [p (Percentile.)
        _ (.setData p (double-array values))]
    (.evaluate p (double percentile))))
