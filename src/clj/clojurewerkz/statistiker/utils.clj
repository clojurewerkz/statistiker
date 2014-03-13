(ns clojurewerkz.statistiker.utils)

(defn- add-unique-id-single
  [a b]
  (assoc a :unique-id b))

(defn add-unique-id
  "Adds unique 0-based incremented id field to each map in `coll`. Field
   is `:unique-id`."
  [coll]
  (map add-unique-id-single coll (iterate inc 0)))
