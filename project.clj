(defproject structured-data "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [iloveponies.tests/structured-data "0.1.0-SNAPSHOT"]
                 [proto-repl "0.3.1"]]
  :profiles {:dev {:plugins [[lein-midje "3.1.1"]]}})
