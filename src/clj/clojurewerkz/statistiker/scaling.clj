(ns clojurewerkz.statistiker.scaling
  (:require [clojurewerkz.statistiker.summary :as summary]))

(defn rescale
  "Linear transformation"
  [x]
  (let [xmin        (apply min x)
        xmax        (apply max x)
        scale-fn (fn [x] (double (/ (- x xmin) (- xmax xmin))))]
    (mapv scale-fn x)))

(defn standartise
  [x]
  (let [xmean    (summary/mean x)
        xsd      (summary/sd x)
        scale-fn (fn [x] (double (/ (- x xmean) xsd)))]
    (map scale-fn x)))
