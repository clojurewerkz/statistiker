(ns clojurewerkz.statistiker.transform.fft
  (:import [org.apache.commons.math3.transform DftNormalization]
           [org.apache.commons.math3.transform FastFourierTransformer TransformType]))

(def dft-normalizations
  {:standard DftNormalization/STANDARD
   :unitary DftNormalization/UNITARY})

(def transform-types
  {:forward TransformType/FORWARD
   :inverse TransformType/INVERSE})

(defn +=bit-shift-right
  [i n]
  (bit-or i (bit-shift-right i n)))

(defn adjust-to-p2
  [num]
  (-> num
      dec
      (+=bit-shift-right 1)
      (+=bit-shift-right 2)
      (+=bit-shift-right 4)
      (+=bit-shift-right 8)
      (+=bit-shift-right 16)
      inc))

(defn zero-fill
  [ds new-size]
  (if (> new-size (count ds))
    (concat ds (into [] (repeat (- new-size (count ds)) 0)))
    ds))

(defn transform
  [data transform-type dft-normalization]
  (let [transformer (FastFourierTransformer. (get dft-normalizations dft-normalization))
        data        (zero-fill data (adjust-to-p2 (count data)))]
    (map (fn [complex idx]
           {:real (.getReal complex) :imaginary (.getImaginary complex) :abs (.abs complex) :idx idx})
         (.transform transformer (double-array (map double data)) (get transform-types transform-type))
         (iterate inc 0))))
