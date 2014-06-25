(ns clojurewerkz.statistiker.clustering.dbscan
  (:import [org.apache.commons.math3.ml.clustering DBSCANClusterer])
  (:require [clojurewerkz.statistiker.distance :as distance]
            [clojurewerkz.statistiker.utils    :as u]
            ))

(defn- ^DBSCANClusterer clusterer
  ([eps min-points]
     (DBSCANClusterer. eps min-points))
  ([eps min-points distance-measure]
     (DBSCANClusterer. eps min-points (get distance/distance-measure-instances distance-measure))))

(defn cluster
  [initial eps min-points]
  (let [clusterer (clusterer eps min-points)]
    (->> initial
         (map u/double-point)
         (.cluster clusterer)
         (map #(hash-map :points (map (fn [a]
                                        (with-meta (vec (.getPoint a))
                                          (.getMetadata a)))
                                      (.getPoints %)))))))

(defn cluster-by
  "Clusters the hashmaps by fields. Fields should be given as vector. Field values
   should be numerical.

   Resulting hashmap will be returned with :cluster-id field that identifies the cluster"
  [data fields eps min-points]
  (let [vectors  (u/prepare-vectors fields data)
        clusters (map :points (cluster vectors eps min-points))]
    (u/unmeta-clusters clusters)))
