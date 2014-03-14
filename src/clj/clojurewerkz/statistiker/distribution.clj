(ns clojurewerkz.statistiker.distribution
  (:import [org.apache.commons.math3.distribution
            NormalDistribution
            LogNormalDistribution
            PoissonDistribution]))

(defn- distribution
  [gen]
  ((fn sample []
     (cons (.sample gen) (lazy-seq (sample))))))

(defn normal-distribution
  [mean sd]
  (distribution (NormalDistribution. mean sd)))

(defn poisson-distribution
  [mean eps max-iter]
  (distribution (PoissonDistribution. (double mean) (double eps) (int max-iter))))

(defn log-normal-distribution
  [scale shape inverse-accuracy]
  (distribution (LogNormalDistribution. (double scale)
                                        (double shape)
                                        (double inverse-accuracy))))
