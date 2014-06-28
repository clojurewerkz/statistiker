(ns clojurewerkz.statistiker.functions
  (:require [clojurewerkz.statistiker.fast-math :as fm])
  (:import [org.apache.commons.math3.analysis MultivariateFunction]
           [org.apache.commons.math3.optimization.direct BOBYQAOptimizer]
           [org.apache.commons.math3.optimization GoalType InitialGuess]))

(defn- line
  [intercept slope]
  (fn [x]
    (+ intercept (* slope x))))

(defn fn->multivariate-function
  [f]
  (reify MultivariateFunction
    (value [_ v]
      (apply f (vec v)))))

(defn linear-fn
  "Linear function for optimizing least squares for linear regression and so forth"
  [data]
  (fn->multivariate-function
   (fn [intercept slope]
     (let [f   (line intercept slope)
           res (->> data
                    (map (fn [[x y]]
                           (fm/sqr
                            (- y (f x)))))
                    (reduce +))]
       res))))
