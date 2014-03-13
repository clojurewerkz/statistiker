(ns clojurewerkz.statistiker.entropy)

(defn shannon-entropy
  "Shannon entropy measure.

   Larger "
  [v]
  (let [sum (reduce + v)]
    (->> v
         (map (fn shannon-entropy-step [i]
                (let [pi (/ i sum)]
                  (* pi (Math/log pi)))))
         (reduce +)
         (* -1))))
