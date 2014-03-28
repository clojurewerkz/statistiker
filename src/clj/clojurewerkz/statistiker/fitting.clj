(ns clojurewerkz.statistiker.fitting
  (:import [org.apache.commons.math3.analysis.function Gaussian]
           [org.apache.commons.math3.fitting GaussianFitter]
           [org.apache.commons.math3.optim.nonlinear.vector.jacobian LevenbergMarquardtOptimizer]))

(defn gaussian-fitting
  [v]
  (let [inst   (GaussianFitter. (LevenbergMarquardtOptimizer.))]
    (doseq [[x y] v]
      (.addObservedPoint inst x y))
    (vec (.fit inst))))

(defn gaussian-function
  [norm mean sigma]
  (Gaussian. norm mean sigma))
