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

(defn cluster-by
  "Clusters the hashmaps by fields. Fields should be given as vector. Field values
   should be numerical.

   Resulting hashmap will be returned with :cluster-id field that identifies the cluster"
  [data fields]
  (let [vectors  (map (fn [point]
                        (with-meta
                          (into [] (for [field fields] (get point field)))
                          point))
                      data)
        clusters (map :points (cluster vectors 5 200))]
    (->> clusters
         (map vector (iterate inc (int 0)))
         (map (fn [[cluster points]]
                (map #(assoc (meta %) :cluster-id cluster) points)))
         ;; mapcat identity?
         flatten)))
