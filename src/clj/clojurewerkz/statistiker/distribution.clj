(ns clojurewerkz.statistiker.distribution
  (:import [org.apache.commons.math3.distribution
            NormalDistribution
            LogNormalDistribution
            MultivariateNormalDistribution
            PoissonDistribution
            BinomialDistribution]))

(defn- distribution
  ([gen]
     ((fn sample []
        (cons (.sample gen) (lazy-seq (sample))))))
  ([gen conv]
     ((fn sample []
        (cons (conv (.sample gen)) (lazy-seq (sample)))))))

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

(defn binomial-distribution
  [trials p]
  (distribution (BinomialDistribution. (int trials)
                                       (double p))))

(defn multivariate-normal-distribution
  [means covariances]
  (distribution (MultivariateNormalDistribution. (double-array means)
                                                 (into-array (map double-array covariances)))
                vec))
