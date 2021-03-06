(ns clojurewerkz.statistiker.fitting
  (:require [clojurewerkz.statistiker.optimization :as optim]
            [clojurewerkz.statistiker.functions    :as funk])
  (:import [org.apache.commons.math3.analysis.function Gaussian]
           [org.apache.commons.math3.fitting GaussianFitter CurveFitter HarmonicFitter PolynomialFitter]))

(defn- make-fitter
  [fitter-klass v]
  (let [inst (eval `(new ~fitter-klass (optim/make-levenberg-marquardt-optimizer)))]
    (doseq [[x y] v]
      (.addObservedPoint inst x y))
    inst))

(defn gaussian-fitter
  [v]
  (let [fitter (make-fitter GaussianFitter v)]
    (vec (.fit fitter))))

(defn curve-fitter
  [v f initial-guess]
  (let [fitter (make-fitter CurveFitter v)]
    (vec (.fit fitter f (double-array initial-guess)))))

(defn polynomial-fitter
  [v max-eval initial-guess]
  (let [fitter (make-fitter PolynomialFitter v)]
    (vec (.fit fitter (int max-eval) (double-array initial-guess)))))

(defn harmonic-fitter
  [v initial-guess]
  (let [fitter (make-fitter HarmonicFitter v)]
    (vec (.fit fitter (double-array initial-guess)))))

(defn fit
  "Extract x and y from dataset, and compose an approximated, fitted dataset from interpolated points, taking
   `steps` points."
  [dss x y fitter-ctor function-ctor steps]
  (let [prepared (map (fn [i] [(get i x)
                              (get i y)]) dss)
        min-x    (reduce min (map (fn [i] (get i x)) dss))
        max-x    (reduce max (map (fn [i] (get i x)) dss))
        step     (/ (- max-x min-x) steps)
        params   (fitter-ctor prepared)
        f        (function-ctor params)]
    (mapv
     (fn [x-val]
       {x x-val y (.value f (double x-val))})
     (take (inc steps) (iterate #(+ % step) min-x)))))
