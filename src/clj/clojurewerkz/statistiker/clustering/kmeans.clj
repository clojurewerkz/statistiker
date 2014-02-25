(ns clojurewerkz.statistiker.clustering.kmeans
  (:import [org.apache.commons.math3.ml.clustering KMeansPlusPlusClusterer]
           [clojurewerkz.statistiker DoublePointWithMeta]))

(defn double-point
  [nums]
  (DoublePointWithMeta. (meta nums) (double-array nums)))

(defn- ^KMeansPlusPlusClusterer clusterer
  ([k max-iter]
     (KMeansPlusPlusClusterer. k max-iter))
  ([k max-iter distance-measure]
     (KMeansPlusPlusClusterer. k max-iter distance-measure)))

(defn cluster
  [initial k max-iter]
  (let [clusterer (clusterer k max-iter)]
    (->> initial
         (map double-point)
         (.cluster clusterer)
         (map #(hash-map :center (vec (.getPoint (.getCenter %)))
                         :points (map (fn [a] (with-meta (vec (.getPoint a))
                                               (.getMetadata a)))
                                      (.getPoints %)))))))
