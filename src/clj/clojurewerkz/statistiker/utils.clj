(ns clojurewerkz.statistiker.utils
  (:import [java.util TreeMap]
           [clojurewerkz.statistiker DoublePointWithMeta]))

(defn- add-unique-id-single
  [a b]
  (assoc a :unique-id b))

(defn add-unique-id
  "Adds unique 0-based incremented id field to each map in `coll`. Field
   is `:unique-id`."
  [coll]
  (map add-unique-id-single coll (iterate inc 0)))

(defn to-sorted-map
  [m]
  (TreeMap. m))

(defn map-groups
  [f groups]
  (into {}
        (for [[group-id items] groups]
          [group-id (f items)])))

(defn select-keys-order-dependent
  [m keys]
  (reduce (fn [acc key] (conj acc (get m key))) [] keys))


;;
;; Clustering
;;

(defn double-point
  [nums]
  (DoublePointWithMeta. (meta nums) (double-array nums)))

(defn prepare-vectors
  [fields data]
  (map #(with-meta (select-keys-order-dependent % fields) %) data))

(defn unmeta-clusters
  [clusters]
  (->> clusters
       (map vector (iterate inc (int 0)))
       (map (fn [[cluster points]]
              (map #(assoc (meta %) :cluster-id cluster) points)))
       (mapcat identity)))
