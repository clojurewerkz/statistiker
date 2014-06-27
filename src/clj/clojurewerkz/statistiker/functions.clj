(ns clojurewerkz.statistiker.functions
  (:require [clojurewerkz.statistiker.fast-math :as fm])
  (:import [org.apache.commons.math3.analysis MultivariateFunction]
           [org.apache.commons.math3.optimization.direct BOBYQAOptimizer]
           [org.apache.commons.math3.optimization GoalType InitialGuess]))

(defn line
  [intercept slope]
  (fn [x]
    (+ intercept (* slope x))))

(defn linear-fn
  "Linear function for optimizing least squares for linear regression and so forth"
  [data]
  (reify MultivariateFunction
    (value [_ v]
      (let [v   (vec v)
            f   (line (first v)
                      (second v))
            res (->> data
                     (map (fn [[x y]]
                            (fm/sqr
                             (- y (f x)))))
                     (reduce +))]
        (println (first v) (second v) res)
        res
        ))))


(def optim (BOBYQAOptimizer. 4))

(defn a
  []
  (let [res               (.optimize optim
                                     (int 300)
                                     (linear-fn [[0 0] [1 1] [2 2] [3 3] [4 4]])
                                     GoalType/MINIMIZE
                                     (double-array [0 0.8])
                                     )
        [intercept slope] (vec (.getPoint res))]
    {:intercept intercept
     :slope slope}))
