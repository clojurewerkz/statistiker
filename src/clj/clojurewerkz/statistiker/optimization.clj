(ns clojurewerkz.statistiker.optimization
  (:require [clojurewerkz.statistiker.fast-math :as fm]
            [clojurewerkz.statistiker.functions :as funk])

  (:import [org.apache.commons.math3.analysis MultivariateFunction]
           [org.apache.commons.math3.optimization.direct BOBYQAOptimizer]
           [org.apache.commons.math3.optimization GoalType InitialGuess]))

(def goal-types
  {:minimize GoalType/MINIMIZE
   :maximize GoalType/MAXIMIZE})

(defn optimize-bobyqa
  "Optimize (multivariate) function `f` with Bound Optimization BY Quadratic Approximation"
  [interpolation-points max-evaluations f goal-type initial-guess]
  (let [optimizer (BOBYQAOptimizer. 4)
        res       (.optimize optimizer
                             (int max-evaluations)
                             f
                             (get goal-types goal-type)
                             (double-array initial-guess))]
    {:point (vec (.getPoint res))
     :value (.getValue res)}))

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
