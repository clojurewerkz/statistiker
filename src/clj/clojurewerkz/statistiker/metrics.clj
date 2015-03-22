(ns clojurewerkz.statistiker.metrics
  (:require [clojure.math.combinatorics       :refer [cartesian-product]]
            [clojurewerkz.statistiker.entropy :refer [shannon-entropy]]
            [clojurewerkz.statistiker.utils   :refer [factorial safe-log safe-shannon-entropy]]))

(defn mutual-information
  "The Mutual Information is a measure of the similarity between two labels of the
   same data.

   Where `P(i)` is the probability of a random sample occurring in cluster `U_i` and `P'(j)`
   is the probability of a random sample occurring in cluster V_j, the Mutual
   Information between clusterings U and V is given as:

       MI(U,V)=\\sum_{i=1}^R \\sum_{j=1}^C P(i,j)\\log\\frac{P(i,j)}{P(i)P'(j)}

   It takes:
     * sequence of golden standard cluster labels (e.g,. [1 1 2 2 4 4])
     * and a set of predicted labels (e.g., [1 1 1 2 4 4])

   This metric is independent of the absolute values of the labels: a permutation of the
   class or cluster label values won’t change the score value in any way."
  [labels-u labels-v]
  {:pre [(= (count labels-u) (count labels-v))
         (not (empty? labels-u))
         (not (empty? labels-v))
         (not-any? coll? labels-u)
         (not-any? coll? labels-v)]}
  (let [freq-u        (frequencies labels-u)
        freq-v        (frequencies labels-v)
        freq-combined (->> (map vector labels-u labels-v)
                           frequencies)
        labels-count  (count labels-u)
        norm-n        (zipmap (keys freq-combined)
                              (map #(/ % labels-count) (vals freq-combined)))]
    (->> norm-n
         (map (fn [[[i j] frequency]]
                (* frequency
                   (safe-log (/ frequency
                                (/ (* (freq-u i) (freq-v j))
                                   (* labels-count labels-count)))))))
         (reduce +))))

(defn p-contingency-cell
  "Calculate probability for one cell of a permuted contingency matrix."
  [labels-count lables-u labels-v [i j nij]]
  (* (/ nij labels-count)
     (safe-log (/ (* labels-count nij)  (* (lables-u i) (labels-v j))))
     (/
      (*' (factorial (lables-u i))
          (factorial (labels-v j))
          (factorial (- labels-count (lables-u i)))
          (factorial (- labels-count (labels-v j))))
      (*' (factorial labels-count)
          (factorial nij)
          (factorial (- (lables-u i) nij))
          (factorial (- (labels-v j) nij))
          (factorial (+ (- labels-count (lables-u i) (labels-v j)) nij))))))

(defn- cell-triples
  "Generates set of indices for permutation model when calculating expected mutual information."
  [labels-count freq-u freq-v i j]
  (let [start (max (- (+ (freq-u i) (freq-v j)) labels-count) 0)
        end   (min (freq-u i) (freq-v j))]
    (map #(vector i j %) (range start (inc end)))))

(defn- expected-mututal-information
  [labels-u labels-v]
  (let [freq-u       (frequencies labels-u)
        freq-v       (frequencies labels-v)
        labels-count (count labels-u)
        cells        (cartesian-product (distinct labels-u) (distinct labels-v))]
    (->> cells
         (map (fn[[i j]]
                (cell-triples labels-count freq-u freq-v i j)))
         (reduce concat)
         (map #(p-contingency-cell labels-count freq-u freq-v %))
         (reduce +))))

(defn adjusted-mutual-information
  "Adjusted Mutual Information (AMI) is an adjustment of the Mutual Information (MI)
   score to account for chance.

       AMI(U, V) = [MI(U, V) - E(MI(U, V))] / [max(H(U), H(V)) - E(MI(U, V))]

   It takes

     * a sequence of golden standard cluster labels (e.g,. [1 1 2 2 4 4])
     * and a set of predicted labels (e.g., [1 1 1 2 4 4])

   This metric is independent of the absolute values of the labels: a permutation of the class
   or cluster label values won’t change the score value in any way."
  [labels-u labels-v]
  {:pre [(= (count labels-u) (count labels-v))
         (not-any? coll? labels-u)
         (not-any? coll? labels-v)]}
  (if (or (empty? labels-u)
          (= 1
             (count (distinct labels-u))
             (count (distinct labels-v))) ; only one cluster
          (= (count labels-u)
             (count (distinct labels-u))
             (count (distinct labels-v)))) ; every point is a singleton cluster
    1.0 ; special limit cases
    (let [mutual-info          (mutual-information labels-u labels-v)
          expected-mutual-info (expected-mututal-information labels-u labels-v)
          h-labels-u           (safe-shannon-entropy (vals (frequencies labels-u)))
          h-labels-v           (safe-shannon-entropy (vals (frequencies labels-v)))]
      (/ (- mutual-info expected-mutual-info)
         (- (max h-labels-u h-labels-v) expected-mutual-info)))))
