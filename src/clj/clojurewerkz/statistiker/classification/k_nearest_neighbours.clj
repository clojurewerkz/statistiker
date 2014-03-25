(ns clojurewerkz.statistiker.classification.k-nearest-neighbours
  (:require [clojurewerkz.statistiker.distance :as distance]))

(defn make-model
  [data]
  (->> data
       (map (fn [[label items]]
              (mapv #(vector label %) items)))
       (mapcat identity)
       vec))

(defn classify
  ([model item k]
     (classify model item k distance/euclidean-distance))
  ([model item k distance-fn]
     (->> model
          (map (fn [[label model-item]] [label (distance-fn item model-item)]))
          (sort-by second)
          (take k))))

(defn best-match
  [classified]
  (->> classified
       (map first)
       (frequencies)
       vec
       (sort-by second >)
       ffirst))
