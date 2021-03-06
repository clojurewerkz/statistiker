(defproject clojurewerkz/statistiker "0.1.0-SNAPSHOT"
  :description "FIXME: write description"

  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure              "1.6.0"]
                 [org.clojure/math.combinatorics   "0.1.1"]
                 [org.apache.commons/commons-math3 "3.4.1"]
                 [net.mikera/core.matrix           "0.33.2"]
                 [prismatic/schema                 "0.2.2"]]
  :core.typed {:check [clojurewerkz.statistiker.optimization]}
  :source-paths       ["src/clj"]
  :java-source-paths  ["src/java"]
  :test-paths         ["test/clj"])
