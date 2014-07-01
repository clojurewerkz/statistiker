(ns clojurewerkz.statistiker.distribution
  (:import [org.apache.commons.math3.distribution
            NormalDistribution
            LogNormalDistribution
            MultivariateNormalDistribution
            PoissonDistribution
            BinomialDistribution
            ChiSquaredDistribution
            ExponentialDistribution
            GammaDistribution
            HypergeometricDistribution]))

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

(defn chi-suqared-distribution
  [degrees-of-freedom inverse-cum-accuracy]
  (distribution (ChiSquaredDistribution. (double degrees-of-freedom)
                                         (double inverse-cum-accuracy))))

(defn exponential-distribution
  [mean inverse-cum-accuracy]
  (distribution (ExponentialDistribution. (double mean)
                                          (double inverse-cum-accuracy))))

(defn gamma-distribution
  [shape scale]
  (distribution (GammaDistribution. (double shape)
                                    (double scale))))

(defn hypergeometric-distribution
  [population-size number-success sample-size]
  (distribution (HypergeometricDistribution. (int population-size)
                                             (int number-success)
                                             (int sample-size))))
