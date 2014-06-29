(ns clojurewerkz.statistiker.functions
  (:require [clojurewerkz.statistiker.fast-math :as fm]
            [clojure.core.matrix                :as matrix]
            [clojure.core.matrix.operators      :as ops])

  (:import [org.apache.commons.math3.analysis MultivariateFunction MultivariateVectorFunction]
           [org.apache.commons.math3.optim InitialGuess MaxEval SimpleBounds]
           [org.apache.commons.math3.optim.nonlinear.scalar ObjectiveFunction ObjectiveFunctionGradient GoalType]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv BOBYQAOptimizer] ;; Use optim version, not that one
           ))

(defn- line
  [intercept slope]
  (fn [x]
    (+ intercept (* slope x))))

(defn fn->multivariate-function
  [f]
  (reify MultivariateFunction
    (value [_ v]
      (apply f (vec v)))))

(defn fn->multivariate-vector-function
  [f]
  (reify MultivariateVectorFunction
    (value [_ v]
      (double-array
       (apply f (vec v))))))

(defn objective-function
  [^MultivariateFunction f]
  (ObjectiveFunction. f))

(defn objective-function-gradient
  [^MultivariateFunction f]
  (ObjectiveFunctionGradient. f))

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


(defn linear-problem
  [factors target]
  {:objective          (objective-function
                        (fn [& point]
                          (->> (matrix/e* factors point)
                               (map (fn [target-i value]
                                      (- value target-i))
                                    target)
                               (reduce +))))

   :objective-gradient (objective-function-gradient
                        (fn [& point]
                          (let [r (ops/- (matrix/e* factors point)
                                         target)]
                            (matrix/e* (matrix/transpose target)
                                       r
                                       2))))})
