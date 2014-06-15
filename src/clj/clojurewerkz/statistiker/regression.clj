(ns clojurewerkz.statistiker.regression
  (:import [org.apache.commons.math3.stat.regression SimpleRegression])
  (:require [clojurewerkz.statistiker.fast-math :refer :all]))

(defn linear-regression
  "Performs a linear regression"
  ([data]
     (linear-regression first second))
  ([data field1-extractor field2-extractor]
     (let [regression (SimpleRegression. true)
           matrix     (into-array (map double-array (map vector
                                                         (map #(get % field1-extractor) data)
                                                         (map #(get % field2-extractor) data))))]
       (.addData regression matrix)
       {:intercept (.getIntercept regression)
        :slope     (.getSlope regression)})))
