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

(defn fit
  "Extract x and y from dataset, and compose an approximated, fitted dataset from interpolated points, taking
   `steps` points."
  [dss x y steps]
  (let [prepared          (map (fn [i] [(get i x)
                                       (get i y)]) dss)
        min-x             (reduce min (map (fn [i] (get i x)) dss))
        max-x             (reduce max (map (fn [i] (get i x)) dss))
        step              (/ (- max-x min-x) steps)
        [norm mean sigma] (gaussian-fitting prepared)
        f                 (gaussian-function norm mean sigma)]
    (mapv
     (fn [x-val]
       {x x-val y (.value f (double x-val))})
     (take steps (iterate #(+ % step) min-x)))))
