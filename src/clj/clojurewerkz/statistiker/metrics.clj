(ns clojurewerkz.statistiker.metrics
  (:require [clojure.math.combinatorics :refer [cartesian-product]]
            [clojurewerkz.statistiker.entropy :refer [shannon-entropy]]))

;TODO move this to a util NS? Or find a better implementation
(defn factorial
  [x]
  {:pre [(>= x 0)]}
  (if (zero? x)
    1
    (loop [n x f 1]
      (if (= n 1)
        f
        (recur (dec n) (* f n))))))

(defn prot-log
  "Protected-log(x) returns 0 if x <= 0 else returns (log x)"
  [x]
  (if (not (pos? x))
    0
    (Math/log x)))

(defn prot-shannon-entropy
  "Shannon entropy measure (inc. protected-logarithms)."
  [v]
  (let [sum (reduce + v)]
    (->> v
         (map (fn shannon-entropy-step [i]
                (let [pi (/ i sum)]
                  (* pi (prot-log pi)))))
         (reduce +)
         (* -1))))

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
        norm_n (into {} (map (fn[[k v]] [k (/ v N)]) n))
        cells (keys norm_n)]
    (reduce + (map
               (fn [[i j]]
                 (* (norm_n [i j])
                    (prot-log (/ (norm_n [i j])
                                 (/ (* (a i) (b j))
                                    (* N N))))))
               cells))))


(defn inside-fn
  [N a b [i j nij]]
  (* (/ nij N)
     (prot-log (/ (* N nij)  (* (a i) (b j))))
     (/
      (* (factorial (a i)) (factorial (b j)) (factorial (- N (a i))) (factorial (- N (b j))))
      (* (factorial N) (factorial nij) (factorial (- (a i) nij)) (factorial (- (b j) nij)) (factorial (+ (- N (a i) (b j)) nij))))))

(defn triples
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
        norm_n (into {} (map (fn[[k v]] [k (/ v N)]) n))
        cells (keys norm_n)
        combinations (apply concat (map (fn[[i j]] ((partial triples N a b) i j)) cells))]
    (reduce + (map (partial inside-fn N a b) combinations))))


; Adjusted Mutual Information (AMI) is an adjustment of the Mutual Information (MI) score to account for chance.
; AMI(U, V) = [MI(U, V) - E(MI(U, V))] / [max(H(U), H(V)) - E(MI(U, V))]
; It takes a sequence of golden standard cluster labels (e.g,. [1 1 2 2 4 4]) and a set of predicted labels (e.g., [1 1 1 2 4 4])

; This metric is independent of the absolute values of the labels: a permutation of the class
; or cluster label values won’t change the score value in any way.
(defn adjusted_mutual_information
  [U V]
  {:pre [(= (count U) (count V))
         (not-any? coll? U)
         (not-any? coll? V)
         (not (empty? U))
         (not (empty? V))]}
  (let [MI (mutual_information U V)
        EMI (expected_mututal_information U V)
        h_U (prot-shannon-entropy (vals (frequencies U)))
        h_V (prot-shannon-entropy (vals (frequencies V)))
        _ (println (str "MI: " MI " EMI: " EMI " hU: " h_U " hV: " h_V))
        ]
    (if (= MI EMI)
      0   ; Avoid some division by zero errors
      (/ (- MI EMI) (- (max h_U h_V) EMI)))))
