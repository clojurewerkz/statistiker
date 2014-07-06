(ns clojurewerkz.statistiker.optimization-test
  (:import [ org.apache.commons.math3.optim.nonlinear.scalar.gradient NonLinearConjugateGradientOptimizer
            NonLinearConjugateGradientOptimizer$Formula]
           [org.apache.commons.math3.optim MaxEval]
           [org.apache.commons.math3.optim.nonlinear.scalar GoalType]
           [org.apache.commons.math3.optim InitialGuess OptimizationData SimpleValueChecker])
  (:require [clojurewerkz.statistiker.optimization :refer :all]
            [clojurewerkz.statistiker.utils :refer [almost=]]
            [clojurewerkz.statistiker.functions]
            [clojure.test :refer :all]))

(def linear-regression-test-data
  [[1.47 52.21] [1.5 53.12] [1.52 54.48] [1.55 55.84]
   [1.57 57.2] [1.6 58.57] [1.63 59.93] [1.65 61.29]
   [1.68 63.11] [1.7 64.47] [1.73 66.28] [1.75 68.1]
   [1.78 69.92] [1.8 72.19] [1.83 74.46]])

(deftest linear-regression-test
  (let [{:keys [intercept slope]} (linear-regression linear-regression-test-data
                                                     [0 0]
                                                     300)]
    (is (almost= intercept -39 0.1))
    (is (almost= slope 61 0.3))))

(deftest linear-regression-gradient-test
  (let [{:keys [intercept slope]} (optimize-non-conjugate-gradient linear-regression-test-data
                                                                   100 :fletcher-reeves)]

    (is (almost= intercept -39 0.1))
    (is (almost= slope 61 0.3))))
