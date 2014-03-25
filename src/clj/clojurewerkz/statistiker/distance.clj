(ns clojurewerkz.statistiker.distance
  (:import [org.apache.commons.math3.ml.distance CanberraDistance ChebyshevDistance EuclideanDistance ManhattanDistance]))

(def euclidean-distance-instance (EuclideanDistance.))
(def canberra-distance-instance (CanberraDistance.))
(def chebyshev-distance-instance (ChebyshevDistance.))
(def manhattan-distance-instance (ManhattanDistance.))

(defn euclidean-distance
  [a b]
  (.compute euclidean-distance-instance (double-array a) (double-array b)))

(defn canberra-distance
  [a b]
  (.compute canberra-distance-instance (double-array a) (double-array b)))

(defn chebyshev-distance
  [a b]
  (.compute chebyshev-distance-instance (double-array a) (double-array b)))

(defn manhattan-distance
  [a b]
  (.compute manhattan-distance-instance (double-array a) (double-array b)))
