(ns clojurewerkz.statistiker.fast-math
  (:import [org.apache.commons.math3.util FastMath Precision]))

(defn exp
  [v]
  (FastMath/exp (double v)))

(defn pow
  [v power]
  (FastMath/pow (double v) power))

(defn sqrt
  [v]
  (FastMath/sqrt v))

(defn equals
  [x y eps]
  (Precision/equals x y eps))
