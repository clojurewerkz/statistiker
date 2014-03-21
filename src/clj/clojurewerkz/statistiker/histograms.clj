(ns clojurewerkz.statistiker.histograms
  (:import [org.apache.commons.math3.random EmpiricalDistribution]))

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
  (let [hist (make-numerical-histogram max-bins)]
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
