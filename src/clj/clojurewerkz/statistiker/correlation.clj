(ns clojurewerkz.statistiker.correlation
  (:import [org.apache.commons.math3.stat.correlation PearsonsCorrelation])
  (:require [clojure.math.combinatorics :as combo]
            [clojurewerkz.statistiker.transform.fft :as fft]
            [clojurewerkz.statistiker.statistics :refer :all]
            ))

(defn pearsons-correlation
  [v1 v2]
  (let [instance (PearsonsCorrelation.)]
    (.correlation instance (double-array v1) (double-array v2))))

(defn best-repetition-patterns
  [v correlation]
  (let [preprocessed (for [i (range 2 (inc (/ (count v) 2)))]
                       (let [partitions   (partition i v)
                             combinations (combo/combinations partitions 2)]
                         [i (mean (map (fn [[v1 v2]] (pearsons-correlation v1 v2)) combinations))]))]
    (->> preprocessed
         (remove #(.isNaN (last %)))
         (remove #(> 0 (last %)))
         (sort-by last >))))

(defn detrend
  [v cycle]
  (->> v
       (partition cycle)
       (map (fn [s]
              (let [m (apply min s)]
                (map #(- % m) s))))
       (mapcat identity)))
