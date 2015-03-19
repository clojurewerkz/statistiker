(ns clojurewerkz.statistiker.metrics
  (:require [clojure.math.combinatorics :refer [cartesian-product]]
            [clojurewerkz.statistiker.entropy :refer [shannon-entropy]]))

;TODO move this to a util NS? Or find a better implementation
(defn factorial
  [x]
    (loop [n x f 1]
        (if (= n 1)
            f
            (recur (dec n) (* f n)))))

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

;TODO: write this!
(defn expected_mututal_information
  [U V]
   (let [a (frequencies U)
        b (frequencies V)
        n (apply merge-with + (map #(hash-map [%1 %2] 1) U V))
        N (count U)
        ;; TODO: Use a fmap.
        norm_n (into {} (map (fn[[k v]] [k (/ v N)]) n))
        cells (keys norm_n)]
(reduce + (map
            (fn [[i j]]

              (/
               (* (factorial (a i)) (factorial (b j)) (factorial (- N (a i))) (factorial (- N (b j))))
               (* (factorial N) (n [i j])
              )
               cells))))

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
  (let [MI (mutual_information U V)
        EMI (expected_mututal_information U V)
        h_U (shannon-entropy U)
        h_V (shannon-entropy V)]
    (/ (- MI EMI) (- (max h_U h_V) EMI))))
