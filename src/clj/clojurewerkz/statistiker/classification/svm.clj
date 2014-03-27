(ns clojurewerkz.statistiker.classification.svm
  (:import [clojurewerkz.statistiker.libsvm.data SvmProblem SvmNode SvmParameter]
           [clojurewerkz.statistiker.libsvm SVM]))

(def kernel-types
  {:linear       SvmParameter/LINEAR
   :poly         SvmParameter/POLY
   :pre-computed SvmParameter/PRECOMPUTED
   :rbf          SvmParameter/RBF
   :sigmoid      SvmParameter/SIGMOID})

(def svm-types
  {:c-svc       SvmParameter/C_SVC
   :epsilon-svr SvmParameter/EPSILON_SVR
   :nu-svc      SvmParameter/NU_SVC
   :nu-svr      SvmParameter/NU_SVR
   :one-class   SvmParameter/ONE_CLASS})

(def default-params
  {:C            1
   :cache-size   100
   :coef0        0
   :degree       3
   :eps          1e-3
   :gamma        0
   :kernel-type  (:rbf kernel-types)
   :nr-weight    0
   :nu           0.5
   :p            0.1
   :probability  0
   :shrinking    1
   :svm-type     (:c-svc svm-types)
   :weight       (double-array 0)
   :weight-label (int-array 0)})

(defn make-node-vec
  [vector]
  (->> vector
       (mapv (fn [index datapoint]
               (SvmNode. index (double datapoint)))
             (iterate inc 1))
       (into-array)))

(defn make-problem
  [dataset]
  (let [converted (->> dataset
                       vec
                       (map (fn [[label vectors]]
                              (->> vectors
                                   (map #(vector label (make-node-vec %))))))
                       (mapcat identity))
        labels (mapv first converted)
        nodes (mapv second converted)]
    (SvmProblem. (double-array labels)
                 (into-array nodes))))

(defn make-params
  [dataset & {:as options}]

  (let [params (SvmParameter.)]
    (doseq [[key val] (merge default-params options)]
      (clojure.lang.Reflector/setInstanceField params (clojure.string/replace (name key) "-" "_") val))
    (set! (.gamma params) (/ 1.0 13))
    params))

(defn train-model
  [^SvmProblem problem ^SvmParameter params]
  (SVM/svm_train problem params))

(defn classify
  [model point]
  (SVM/svm_predict model (make-node-vec point)))
