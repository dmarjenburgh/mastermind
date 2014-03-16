(defproject mastermind "0.1.0-SNAPSHOT"
  :description "A program that plays Mastermind"
  :url "https://github.com/dmarjenburgh/mastermind"
  :license {:name "MIT"
            :url "http://www.opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [slingshot "0.10.3"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [criterium "0.4.3"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]}}
  :main mastermind.core)
