(defproject n-bodies "0.1.0-SNAPSHOT"
  :description "Solves the N-Bodies problem concerning the attraction of forces"
  :url nil
  :license {:name "MIT"
            :url "opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[speclj "2.5.0"]]}}
  :plugins [[speclj "2.5.0"]]
  :test-paths ["spec"]
  :main n-bodies.core
)
