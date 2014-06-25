(ns clojurewerkz.statistiker.scaling
  (:require [clojurewerkz.statistiker.statistics :refer [mean sd]]
            [clojurewerkz.statistiker.fast-math :refer [sqr]]))

(defn make-rescale-range-fn
  [x xmin xmax]
  (let [orig-min  (apply min x)
        orig-max  (apply max x)
        orig-diff (- orig-max orig-min)
        diff      (- xmax xmin)]
    (fn [x]
      (double (+ (/ (* (- x orig-min)
                       diff)
                    orig-diff)
                 xmin
                 )))))

(defn make-rescale-fn
  [x]
  (let [xmin (apply min x)
        xmax (apply max x)
        diff (- xmax xmin) ]
    (fn [x] (double (/ (- x xmin) diff)))))

(defn rescale
  [x]
  (mapv (make-rescale-fn x) x))

(defn rescale-range
  [x xmin xmax]
  (mapv (make-rescale-range-fn x xmin xmax) x))

(defn make-standartise-fn
  [x]
  (let [xmean    (mean x)
        xsd      (sd x)]
    (fn [x] (double (/ (- x xmean) xsd)))))

(defn standartise
  [x]
  (mapv (make-standartise-fn x) x))

(defn make-l1-normalize-fn
  [x]
  (let [sum (->> x
                 (map #(Math/abs %))
                 (reduce +))]
    #(/ % sum)))

(defn l1-normalize
  "L1-normalize (divide each element by sum of absolute values)."
  [x]
  (mapv (make-l1-normalize-fn x) x))

(defn make-l2-normalize-fn
  [x]
  (let [sum (->> x
                 (map #(sqr %))
                 (reduce +))]
    #(/ % sum)))

(defn l2-normalize
  "L2-normalize (divide each element by sum of squares)"
  [x]
  (mapv (make-l2-normalize-fn x) x))

(defn scale-feature
  ([maps key scaled-key scale-fn-factory]
     (let [extracted (map #(get % key) maps)
           scale-f   (scale-fn-factory extracted)
           scaled    (map scale-f extracted)]
       (map (fn [v scaled]
              (assoc v scaled-key scaled)) maps scaled)))
  ([maps key scale-fn-factory]
     (scale-feature maps key key scale-fn-factory)))
