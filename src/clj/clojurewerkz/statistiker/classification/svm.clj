(ns clojurewerkz.statistiker.classification.svm)

(doseq [[k vals] (group-by first a)]

  (comment
    (spit (str "/tmp/" k)
          (str "[" (clojure.string/join " " (mapv #(vec (next %)) vals)) "]")
          ;; :append true
          )))
