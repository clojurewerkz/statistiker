(ns clojurewerkz.statistiker.fast-math
  (:import [org.apache.commons.math3.util FastMath Precision])
  (:require [clojure.core.typed          :as cct :refer [ann ann-record check-ns]]))

(ann exp (cct/IFn [Number -> Double]))
(ann pow (cct/IFn [Number Integer -> Double]))
(ann sqr (cct/IFn [Number -> Number]))
(ann sqrt (cct/IFn [Double -> Double]))
(ann equals (cct/IFn [Number Number Number -> Boolean]))

(defn exp
  [v]
  (FastMath/exp (double v)))

(defn pow
  [v power]
  (FastMath/pow (double v) power))

(defn sqr
  [v]
  (FastMath/pow (double v) 2))

(defn sqrt
  [v]
  (FastMath/sqrt v))

(defn equals
  [x y eps]
  (Precision/equals (double x) (double y) (double eps)))
