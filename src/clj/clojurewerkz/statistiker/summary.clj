(ns clojurewerkz.statistiker.summary
  (:require [clojurewerkz.statistiker.fast-math :refer :all]))

(defn mean
  [values]
  (float (/ (reduce + values) (count values))))

(defn variance
  [values]
  (let [m (mean values)]
    (/ (->> values
            (map #(pow (- % m) 2))
            (reduce +))
       (- (count values) 1))))

(defn standard-deviation
  [values]
  (sqrt (variance values)))

(defn geometric-mean
  [values]
  (pow (reduce * values) (/ 1 (count values))))
