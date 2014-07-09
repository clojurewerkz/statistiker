(ns clojurewerkz.statistiker.functions
  (:require [clojurewerkz.statistiker.fast-math :as fm]
            [clojure.core.matrix                :as matrix]
            [clojure.core.matrix.operators      :as ops]
            [schema.core                        :as s])

  (:import [clojure.lang IFn]
           [org.apache.commons.math3.analysis.function Gaussian Gaussian$Parametric]
           [org.apache.commons.math3.analysis.polynomials PolynomialFunction]
           [org.apache.commons.math3.analysis MultivariateFunction MultivariateVectorFunction ParametricUnivariateFunction]
           [org.apache.commons.math3.optim.nonlinear.scalar ObjectiveFunction ObjectiveFunctionGradient]))

(defn ^Number line
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


(defn least-squares-problem
  "Least squares problem: https://en.wikipedia.org/wiki/Linear_least_squares_(mathematics)#The_general_problem"
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
                         ;; M = (X ^ T * X)
                         ;; b = (X ^ T * y)
                         ;; beta = M^-1 * b
                         (let [ft (matrix/transpose factors)
                               m! (matrix/inverse (matrix/dot ft factors))
                               b  (matrix/dot ft target)]

                           (ops/- (matrix/mmul m! b)
                                  point)))))))

;; (curve-fitter [[1 1] [2 2] [3 3]] (funk/gaussian-function) [0 0 1 ])
(defn gaussian-function
  ([]
     (Gaussian$Parametric.))
  ([norm mean sigma]
     (Gaussian. norm mean sigma)))

(comment (defn polynomial-function
           []
           (PolynomialFunction. norm mean sigma)))

(defn wrap-function
  "Returns a clojure Fn that wraps Function"
  [funk]
  (fn [x]
    (.value funk (double x))))

(defn to-parametric-fn
  [identity]
  (reify ParametricUnivariateFunction
   (value [this x params]
     (.value (apply identity params) (double x)))
   (gradient [this x params]
     (.gradient (apply identity params) (double x)))))
