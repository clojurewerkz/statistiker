(ns clojurewerkz.statistiker.metrics
  (:require [clojure.math.combinatorics       :refer [cartesian-product]]
            [clojurewerkz.statistiker.entropy :refer [shannon-entropy]]
            [clojurewerkz.statistiker.utils   :refer [factorial safe-log prot-shannon-entropy]]))

; The Mutual Information is a measure of the similarity between two labels of the same data.
; Where P(i) is the probability of a random sample occurring in cluster U_i and P'(j) is the probability of a random sample
; occurring in cluster V_j, the Mutual Information between clusterings U and V is given as:
; MI(U,V)=\sum_{i=1}^R \sum_{j=1}^C P(i,j)\log\frac{P(i,j)}{P(i)P'(j)}
; It takes a sequence of golden standard cluster labels (e.g,. [1 1 2 2 4 4]) and a set of predicted labels (e.g., [1 1 1 2 4 4])

; This metric is independent of the absolute values of the labels: a permutation of the class or cluster label
; values won’t change the score value in any way.
(defn mutual-information
  [labels-u labels-v]
  {:pre [(= (count labels-u) (count labels-v))
         (not (empty? labels-u))
         (not (empty? labels-v))]}
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
  [N a b [i j nij]]
  (* (/ nij N)
     (safe-log (/ (* N nij)  (* (a i) (b j))))
     (/
      (*' (factorial (a i)) (factorial (b j)) (factorial (- N (a i))) (factorial (- N (b j))))
      (*' (factorial N) (factorial nij) (factorial (- (a i) nij)) (factorial (- (b j) nij)) (factorial (+ (- N (a i) (b j)) nij))))))

(defn cell-triples
  "Generates set of indices for permutation model when calculating expected mutual information."
  [N a b i j]
  (let [start (max (- (+ (a i) (b j)) N) 0)
        end (min (a i) (b j))]
    (map #(vector i j %) (range start (inc end)))))

(defn expected_mututal_information
  [U V]
  (let [a (frequencies U)
        b (frequencies V)
        n (apply merge-with + (map #(hash-map [%1 %2] 1) U V))
        N (count U)
        norm-n (into {} (map (fn[[k v]] [k (/ v N)]) n))
        cells (cartesian-product  (distinct U) (distinct V))
        combinations (apply concat (map (fn[[i j]] ((partial cell-triples N a b) i j)) cells))]
    (reduce + (map (partial p-contingency-cell N a b) combinations))))


                                        ; Adjusted Mutual Information (AMI) is an adjustment of the Mutual Information (MI) score to account for chance.
                                        ; AMI(U, V) = [MI(U, V) - E(MI(U, V))] / [max(H(U), H(V)) - E(MI(U, V))]
                                        ; It takes a sequence of golden standard cluster labels (e.g,. [1 1 2 2 4 4]) and a set of predicted labels (e.g., [1 1 1 2 4 4])

                                        ; This metric is independent of the absolute values of the labels: a permutation of the class
                                        ; or cluster label values won’t change the score value in any way.
(defn adjusted_mutual-information
  [U V]
  {:pre [(= (count U) (count V))
         (not-any? coll? U)
         (not-any? coll? V)]}
  (if (or (empty? U)
          (= 1 (count (distinct U)) (count (distinct V)))          ;only one cluster
          (= (count U) (count (distinct U)) (count (distinct V)))) ;every point is a singleton cluster
    1.0    ;special limit cases
    (let [MI (mutual-information U V)
          EMI (expected_mututal_information U V)
          h_U (prot-shannon-entropy (vals (frequencies U)))
          h_V (prot-shannon-entropy (vals (frequencies V)))]
      (/ (- MI EMI) (- (max h_U h_V) EMI)))))
