(ns clojurewerkz.statistiker.clustering.dbscan
  (:import [org.apache.commons.math3.ml.clustering DBSCANClusterer])
  (:require [clojurewerkz.statistiker.distance :as distance]
            [clojurewerkz.statistiker.utils :refer :all]))

(defn- ^DBSCANClusterer clusterer
  ([eps min-points]
     (DBSCANClusterer. eps min-points))
  ([eps min-points distance-measure]
     (DBSCANClusterer. eps min-points distance-measure)))

(defn cluster
  [initial eps min-points]
  (let [clusterer (clusterer eps min-points)]
    (->> initial
         (map double-point)
         (.cluster clusterer)
         (map #(hash-map :points (map (fn [a] (with-meta (vec (.getPoint a))
                                               (.getMetadata a)))
                                      (.getPoints %)))))))
