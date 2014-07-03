(ns clojurewerkz.statistiker.optimization
  (:require [clojurewerkz.statistiker.fast-math :as fm]
            [clojurewerkz.statistiker.functions :as funk])

  (:import [org.apache.commons.math3.optim InitialGuess MaxEval SimpleBounds OptimizationData SimpleValueChecker
            PointValuePair]
           [org.apache.commons.math3.optim.nonlinear.scalar ObjectiveFunction ObjectiveFunctionGradient
            GoalType MultivariateOptimizer]
           [ org.apache.commons.math3.optim.nonlinear.scalar.gradient NonLinearConjugateGradientOptimizer
            NonLinearConjugateGradientOptimizer$Formula]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv BOBYQAOptimizer ]))

(defn- point->map
  [^PointValuePair point]
  {:point (vec (.getPoint point))
   :value (.getValue point)})

(def goal-types
  {:minimize GoalType/MINIMIZE
   :maximize GoalType/MAXIMIZE})

(defn optimize-bobyqa
  "Optimize (multivariate) function `f` with Bound Optimization BY Quadratic Approximation"
  [interpolation-points max-evaluations f goal-type initial-guess]
  (let [optimizer (BOBYQAOptimizer. 4)
        res       (.optimize optimizer
                             (into-array OptimizationData
                                         [(MaxEval. (int max-evaluations))
                                          (funk/objective-function f)
                                          (get goal-types goal-type)
                                          (SimpleBounds/unbounded (count initial-guess))
                                          (InitialGuess. (double-array initial-guess))]))]
    {:point (vec (.getPoint res))
     :value (.getValue res)}))

(def ^:private non-conjugate-gradient-optimizer-formula
  {:fletcher-reeves NonLinearConjugateGradientOptimizer$Formula/FLETCHER_REEVES
   :polak-ribiere   NonLinearConjugateGradientOptimizer$Formula/POLAK_RIBIERE})

(defn optimize-non-conjugate-gradient
  [data max-evaluations formula]
  (let [problem           (funk/two-var-least-squares data)
        optim             (NonLinearConjugateGradientOptimizer. (get non-conjugate-gradient-optimizer-formula formula)
                                                                (SimpleValueChecker. 1e-6, 1e-6))
        res               (.optimize optim
                                     (into-array OptimizationData
                                                 [(MaxEval. (int max-evaluations))
                                                  (:objective problem)
                                                  (:objective-gradient problem)
                                                  GoalType/MINIMIZE
                                                  (InitialGuess. (double-array [0 0]))]
                                                 ))
        {:keys [point]}   (point->map res)
        [intercept slope] point]
    {:intercept intercept
     :slope     slope}))

;;
;; Implementaion
;;

(defn- interpolation-points-avg
  [n]
  (int (/ (+ (+ n 2)
             (/ (* (+ n 1) (+ n 2)) 2))
          2)))

(defn linear-regression
  ([data initial-guess max-iterations]
     (linear-regression data
                        initial-guess
                        max-iterations
                        (interpolation-points-avg (count initial-guess))))
  ([data initial-guess max-iterations interpolation-points ]
     (let [{:keys [point]}   (optimize-bobyqa
                              interpolation-points
                              max-iterations
                              (funk/linear-fn data)
                              :minimize
                              (double-array initial-guess))
           [intercept slope] point]
       {:intercept intercept
        :slope slope})))
