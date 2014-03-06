(ns clojurewerkz.statistiker.time-series.smoothing
  (:require [clojurewerkz.statistiker.summary :refer :all]))

(defn fixed-size-conj
  [size]
  (fn fixed-size-conj-fn [v item]
    (let [c (count v)]
      (if (>= c size)
        (conj (subvec v (- c (- size 1)) c) item)
        (conj v item)))))

(defn linear-smooth-stream
  "Linear smoothing makes a fitted line "
  ([size consume-fn]
     (let [conj-fn (fixed-size-conj size)
           window  (atom [])]
       (fn [i]
         (swap! window conj-fn i)
         (when (= (count @window) size)
           (consume-fn (mean @window))))))
  ([size consume-fn field]
     (let [conj-fn (fixed-size-conj size)
           window  (atom [])]
       (fn [i]
         (swap! window conj-fn (get i field))
         (when (= (count @window) size)
           (consume-fn (assoc i field (mean @window))))))))

(defn linear-smooth-seq
  [window lst]
  (map mean (partition window 1 lst)))
