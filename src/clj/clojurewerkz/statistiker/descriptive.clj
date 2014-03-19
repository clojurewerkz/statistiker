(ns clojurewerkz.statistiker.descriptive
  (:import [org.apache.commons.math3.stat.descriptive.rank Percentile]
           [org.apache.commons.math3.random EmpiricalDistribution]
           [java.util TreeMap])
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

(defn median
  [values]
  (let [p (Percentile.)
        _ (.setData p (double-array values))]
    (.evaluate p (double 50))))

(defn percentiles
  [values percentiles]
  (let [p              (Percentile.)
        _              (.setData p (double-array values))]
    (mapv #(.evaluate p (double %)) percentiles)))

(defn percentile
  [values percentile]
  (let [p              (Percentile.)
        _              (.setData p (double-array values))]
    (.evaluate p (double percentile))))

(defprotocol Histogram
  (add [_ value])
  (get-counts [_]))

(defn- get-diff
  [[i1 i2]]
  (- i2 i1))

(defn- merge-closest
  [m]
  (let [[[q1 q2] _] (->> m
                         keys
                         sort
                         (partition 2 1)
                         (sort-by get-diff))
        k1          (get m q1)
        k2          (get m q2)]
    (-> m
        (dissoc q1)
        (dissoc q2)
        (assoc
            (/ (+ (* q1 k1) (* q2 k2))
               (+ k1 k2))
          (+ k1 k2)))))

(defn make-numerical-histogram
  [max-bins]
  (let [bins (atom {})]
    (reify Histogram
      (add [_ value]
        (swap! bins
               (fn [bins]
                 (if-let [prev (get bins value)]
                   (assoc bins value (inc prev))
                   (loop [bins (assoc bins value 1)]
                     (if (<= (count bins) max-bins)
                       bins
                       (recur (merge-closest bins))))))))
      (get-counts [_] (into {}
                            (mapv (fn [[k v]] [(double k) (double v)]) @bins))))))

(defn numerical-histogram
  [max-bins vals]
  (let [hist (make-numerical-histogram 5)]
    (doseq [i vals]
      (add hist i))
    (get-counts hist)))

(defn empirical-distribution
  [bins data]
  (let [emp-d (EmpiricalDistribution. (int bins))]
    (.load emp-d (double-array (mapv double data)))
     (zipmap
     (.getUpperBounds emp-d)
     (map #(.getN %) (.getBinStats emp-d)))))

(defn to-sorted-map
  [m]
  (TreeMap. m))
