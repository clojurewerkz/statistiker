(ns clojurewerkz.statistiker.functions
  (:require [clojurewerkz.statistiker.fast-math :as fm]
            [clojure.core.matrix                :as matrix]
            [clojure.core.matrix.operators      :as ops]
            [schema.core                        :as s])

  (:import [clojure.lang IFn]
           [org.apache.commons.math3.analysis MultivariateFunction MultivariateVectorFunction]
           [org.apache.commons.math3.optim InitialGuess MaxEval SimpleBounds]
           [org.apache.commons.math3.optim.nonlinear.scalar ObjectiveFunction ObjectiveFunctionGradient GoalType]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv BOBYQAOptimizer] ;; Use optim version, not that one
           ))

(defn line
  "Simple linear funciton:

     f(y) = ax + b"
  [^Number intercept ^Number slope]
  (fn [x]
    (+ intercept (* slope x))))

(defn fn->multivariate-function
  [^IFn f]
  (reify MultivariateFunction
    (value [_ v]
      (apply f (vec v)))))

(defn fn->multivariate-vector-function
  [^IFn f]
  (reify MultivariateVectorFunction
    (value [_ v]
      (double-array
       (apply f (vec v))))))

(defn objective-function
  [^IFn f]
  (ObjectiveFunction. (fn->multivariate-function f)))

(defn objective-function-gradient
  [^IFn f]
  (ObjectiveFunctionGradient. (fn->multivariate-vector-function f)))

;;
;; Functions
;;

(s/defrecord GradientProblem
    [^{:s ObjectiveFunction}         objective-fn
     ^{:s ObjectiveFunctionGradient} objective-fn-gradient])

(defn make-gradient-problem
  [^IFn objective ^IFn gradient]
  (s/validate GradientProblem
              (GradientProblem. (objective-function objective)
                                (objective-function-gradient))))


(defn ^IFn linear-fn
  "Linear function for optimizing least squares for linear regression and so forth"
  [data]
  (fn [^Number intercept ^Number slope]
    (let [f   (line intercept slope)
          res (->> data
                   (map (fn [[x y]]
                          (fm/sqr
                           (- y (f x)))))
                   (reduce +))]
      res)))

(defn ^GradientProblem linear-problem
  [factors target]
  (GradientProblem. (objective-function
                     (fn [& point]
                       (->> (matrix/e* factors point)
                            (map (fn [target-i value]
                                   (- value target-i))
                                 target)
                            (reduce +))))

                    (objective-function-gradient
                     (fn [& point]
                       (let [r (ops/- (matrix/e* factors point)
                                      target)]
                         (matrix/e* (matrix/transpose target)
                                    r
                                    2))))))


(defn two-var-least-squares-vector
  [points]
  (let [factors (->> points (map butlast) (map #(cons 1 %)))
        target  (map last points)]
    (GradientProblem. (objective-function
                       (fn [intercept slope]
                         (let [f   (line intercept slope)
                               res (->> points
                                        (map (fn [[x y]]
                                               (fm/sqr (- y (f x)))))
                                        (reduce +))]
                           res)))

                      (objective-function-gradient
                       (fn [& point]
                         (let [ft (matrix/transpose factors)
                               m! (matrix/inverse (matrix/dot ft factors))
                               b  (matrix/dot ft target)]

                           (ops/- (matrix/mmul m! b)
                                  point)))))))


(defn two-var-least-squares
  [points]
  (GradientProblem. (objective-function
                     (fn [intercept slope]
                       (let [f   (line intercept slope)
                             res (->> points
                                      (map (fn [[x y]]
                                             (fm/sqr (- y (f x)))))
                                      (reduce +))]
                         res)))

                    (objective-function-gradient
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

                         ]))))
