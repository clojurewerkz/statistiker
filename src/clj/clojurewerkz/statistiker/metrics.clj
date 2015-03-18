(ns clojurewerkz.statistiker.metrics
  (:require [clojure.math.combinatorics :refer [cartesian-product]]))

; Adjusted Mutual Information (AMI) is an adjustment of the Mutual Information (MI) score to account for chance.
; AMI(U, V) = [MI(U, V) - E(MI(U, V))] / [max(H(U), H(V)) - E(MI(U, V))]
; It takes a sequence of golden standard cluster labels (e.g,. [1 1 2 2 4 4]) and a set of predicted labels (e.g., [1 1 1 2 4 4])

; This metric is independent of the absolute values of the labels: a permutation of the class
; or cluster label values won’t change the score value in any way.
(defn adjusted_mutual_information
  [U V]
  {:pre [(= (count U) (count V))
         (not-any? coll? U)
         (not-any? coll? V)]}
  0.0)

; The Mutual Information is a measure of the similarity between two labels of the same data.
; Where P(i) is the probability of a random sample occurring in cluster U_i and P'(j) is the probability of a random sample
; occurring in cluster V_j, the Mutual Information between clusterings U and V is given as:
; MI(U,V)=\sum_{i=1}^R \sum_{j=1}^C P(i,j)\log\frac{P(i,j)}{P(i)P'(j)}
; It takes a sequence of golden standard cluster labels (e.g,. [1 1 2 2 4 4]) and a set of predicted labels (e.g., [1 1 1 2 4 4])

; This metric is independent of the absolute values of the labels: a permutation of the class or cluster label
; values won’t change the score value in any way.
(defn mutual_information
  [U V]
  {:pre [(= (count U) (count V))
         (not-any? coll? U)
         (not-any? coll? V)]}
  (let [a (frequencies U)
        b (frequencies V)
        n (apply merge-with + (map #(hash-map [%1 %2] 1) U V))
        N (count U)
        ;; TODO: Use a fmap.
        norm_n (into {} (map (fn[[k v]] [k (/ v N)]) n))
        cells (keys norm_n)]
    (reduce + (map
               (fn [[i j]]
                 (* (norm_n [i j])
                    (Math/log (/ (norm_n [i j])
                                         (/ (* (a i) (b j))
                                            (* N N))))))
               cells))))
