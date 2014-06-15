(ns clojurewerkz.statistiker.statistics
  (:import [org.apache.commons.math3.stat.descriptive.rank Percentile]
           [org.apache.commons.math3.stat StatUtils])
  (:require [clojurewerkz.statistiker.fast-math :refer [sqrt pow]]))

(defn mean
  [values]
  (let [c (count values)]
    (if (> c 0)
      (float (/ (reduce + values) (count values)))
      0)))

(defn variance
  [values]
  (let [m (mean values)]
    (/ (->> values
            (map #(pow (- % m) 2))
            (reduce +))
       (- (count values) 1))))

(defn standard-deviation
  [values]
  (sqrt (variance values)))

(def sd standard-deviation)

(defn geometric-mean
  [values]
  (StatUtils/geometricMean (double-array values)))

(defn mode
  [values]
  (vec (StatUtils/mode (double-array values))))

(defn normalize
  [values]
  (vec (StatUtils/normalize (double-array values))))

(defn product
  [values]
  (StatUtils/product (double-array values)))

(def ^:private percentile-mappings
  {:min    1
   :max    100
   :median 50
   :25     25
   :75     75})

(defn fivenum
  [values]
  (let [p (Percentile.)]
    (.setData p (double-array values))
    (reduce (fn [acc [k v]]
              (assoc acc k (.evaluate p (double v))))
            {}
            percentile-mappings)))

(defn iqr
  [values]
  (let [p              (Percentile.)
        _              (.setData p (double-array values))
        first-quartile (.evaluate p (double 25))
        third-quartile (.evaluate p (double 75))]
    (- third-quartile first-quartile)))

(defn median
  [values]
  (let [p (Percentile.)
        _ (.setData p (double-array values))]
    (.evaluate p (double 50))))

(defn percentiles
  [values percentiles]
  (let [p (Percentile.)
        _ (.setData p (double-array values))]
    (mapv #(.evaluate p (double %)) percentiles)))

(defn percentile
  [values percentile]
  (let [p (Percentile.)
        _ (.setData p (double-array values))]
    (.evaluate p (double percentile))))
