(ns clojurewerkz.statistiker.functions
  (:require [clojurewerkz.statistiker.fast-math :as fm]
            [clojure.core.matrix                :as matrix]
            [clojure.core.matrix.operators      :as ops])

  (:import [org.apache.commons.math3.analysis MultivariateFunction MultivariateVectorFunction]
           [org.apache.commons.math3.optim InitialGuess MaxEval SimpleBounds]
           [org.apache.commons.math3.optim.nonlinear.scalar ObjectiveFunction ObjectiveFunctionGradient GoalType]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv BOBYQAOptimizer] ;; Use optim version, not that one
           ))

(defn line
  "Simple linear funciton:

     f(y) = ax + b"
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
  [f]
  (ObjectiveFunction. (fn->multivariate-function f)))

(defn objective-function-gradient
  [f]
  (ObjectiveFunctionGradient. (fn->multivariate-vector-function f)))

;;
;; Functions
;;

(defn linear-fn
  "Linear function for optimizing least squares for linear regression and so forth"
  [data]
  (fn [intercept slope]
    (let [f   (line intercept slope)
          res (->> data
                   (map (fn [[x y]]
                          (fm/sqr
                           (- y (f x)))))
                   (reduce +))]
      res)))

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


(defn two-var-least-squares
  [points]
  {:objective          (objective-function
                        (fn [intercept slope]
                          (let [f   (line intercept slope)
                                res (->> points
                                         (map (fn [[x y]]
                                                (fm/sqr (- y (f x)))))
                                         (reduce +))]
                            res)))

   :objective-gradient (objective-function-gradient
                        (fn [intercept slope]
                          [ (* 2 (->> points
                                      (map (fn [[x y]]
                                             (*
                                              (- y (+ intercept (* slope x)))
                                              -1)))
                                      (reduce +)))

                            (* 2 (->> points
                                      (map (fn [[x y]]
                                             (*
                                              (- y (+ intercept (* slope x)))
                                              -1
                                              x)))
                                      (reduce +)))
                            ]
                          ))})
