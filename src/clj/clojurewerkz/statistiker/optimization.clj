(ns clojurewerkz.statistiker.optimization
  (:require [clojurewerkz.statistiker.fast-math :as fm]
            [clojurewerkz.statistiker.functions :as funk]
            [schema.macros                      :as sm])

  (:import [clojure.lang IFn]
           [clojurewerkz.statistiker.functions GradientProblem]
           [org.apache.commons.math3.optim InitialGuess MaxEval SimpleBounds
            OptimizationData SimpleValueChecker PointValuePair]
           [org.apache.commons.math3.optim.nonlinear.scalar ObjectiveFunction
            ObjectiveFunctionGradient GoalType MultivariateOptimizer]
           [ org.apache.commons.math3.optim.nonlinear.scalar.gradient NonLinearConjugateGradientOptimizer
            NonLinearConjugateGradientOptimizer$Formula]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv BOBYQAOptimizer]))

;;
;; Constants
;;

(def ^:private goal-types
  {:minimize GoalType/MINIMIZE
   :maximize GoalType/MAXIMIZE})

(def ^:private non-conjugate-gradient-optimizer-formula
  {:fletcher-reeves NonLinearConjugateGradientOptimizer$Formula/FLETCHER_REEVES
   :polak-ribiere   NonLinearConjugateGradientOptimizer$Formula/POLAK_RIBIERE})

;;
;; Constructors
;;

(defn ^BOBYQAOptimizer make-bobyqa-optimizer
  [iteration-points]
  (BOBYQAOptimizer. (int iteration-points)))

(defn ^NonLinearConjugateGradientOptimizer make-ncg-optimizer
  ([formula]
     (make-ncg-optimizer formula 1e-6 1e-6))
  ([formula relative-threshold absolute-threshold]
     (NonLinearConjugateGradientOptimizer. (sm/safe-get non-conjugate-gradient-optimizer-formula formula)
                                           (SimpleValueChecker. (double relative-threshold) (double absolute-threshold)))))


(defn- point->map
  [^PointValuePair point]
  {:point (vec (.getPoint point))
   :value (.getValue point)})

(defn initial-guess
  "Receives a vector of numbers to be used as an initial guess"
  [guess]
  (InitialGuess. (double-array guess)))

;;
;; Optimization Shortcuts
;;


(defn optimize-bobyqa
  "Optimize (multivariate) function `f` with Bound Optimization By Quadratic Approximation"
  [interpolation-points max-evaluations ^IFn f goal-type guess]
  (let [optimizer (make-bobyqa-optimizer 4) ;; Remove hardcoded variable
        res       (.optimize optimizer
                             (into-array OptimizationData
                                         [(MaxEval. (int max-evaluations))
                                          (funk/objective-function f)
                                          (sm/safe-get goal-types goal-type)
                                          (SimpleBounds/unbounded (count guess))
                                          (initial-guess guess)]))]
    (point->map res)))



(defn optimize-non-conjugate-gradient
  ([^IFn objective ^IFn gradient ^Number max-evaluations ^clojure.lang.Keyword formula]
     (optimize-non-conjugate-gradient (funk/make-gradient-problem objective
                                                                  gradient)
                                      max-evaluations
                                      formula))

  ([^GradientProblem problem ^Number max-evaluations ^clojure.lang.Keyword formula]
     (let [optim             (make-ncg-optimizer formula)
           res               (.optimize optim
                                        (into-array OptimizationData
                                                    [(MaxEval. (int max-evaluations))
                                                     (.objective-fn problem)
                                                     (.objective-fn-gradient problem)
                                                     (:minimize goal-types) ;; TODO: use safe-get?
                                                     (initial-guess [0 0])]))]
       (point->map res))))

;;
;; Implementaion
;;


(defn bobyqa-interpolation-points-avg
  [n]
  (int (/ (+ (+ n 2)
             (/ (* (+ n 1) (+ n 2)) 2))
          2)))
