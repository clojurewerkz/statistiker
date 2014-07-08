(ns clojurewerkz.statistiker.regression
  (:import [org.apache.commons.math3.stat.regression SimpleRegression])
  (:require [clojurewerkz.statistiker.optimization :as optim]
            [clojurewerkz.statistiker.functions    :as funk]))

(defn linear-regression
  "Performs a linear regression"
  ([data]
     (linear-regression data first second))
  ([data field1-extractor field2-extractor]
     (let [field1-extractor (if (fn? field1-extractor)
                              field1-extractor
                              #(get % field1-extractor))
           field2-extractor (if (fn? field2-extractor)
                              field2-extractor
                              #(get % field2-extractor))
           regression       (SimpleRegression. true)
           matrix           (into-array (map double-array (map vector
                                                               (map field1-extractor data)
                                                               (map field2-extractor data))))]
       (.addData regression matrix)
       {:intercept (.getIntercept regression)
        :slope     (.getSlope regression)})))

(defn linear-regression2
  "Linear Regression through linear BOBYQA optimizer.

   We do not recommend using linear-regression2 or linear-regression3, since they yield same results
   as a default implementation, although have much poorer performance. They're given here as reference
   implementation of linear and gradient optimisation."
  ([data initial-guess max-iterations]
     (linear-regression2 data
                         initial-guess
                         max-iterations
                         (optim/bobyqa-interpolation-points-avg (count initial-guess))))
  ([data initial-guess max-iterations interpolation-points ]
     (let [{:keys [point]}   (optim/optimize-bobyqa interpolation-points
                                                    max-iterations
                                                    (funk/linear-fn data)
                                                    :minimize
                                                    (double-array initial-guess))
           [intercept slope] point]
       {:intercept intercept
        :slope     slope})))

(defn linear-regression3
  "Linear Regression through Non-Conjugate Gradient Descent."
  [data max-evaluations formula]
  (let [problem           (funk/least-squares-problem data)
        res               (optim/optimize-non-conjugate-gradient problem max-evaluations formula)
        {:keys [point]}   res
        [intercept slope] point]
    {:intercept intercept
     :slope     slope}))
