(defproject structured-data "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [iloveponies.tests/structured-data "0.2.0-SNAPSHOT"]]
  :profiles {:dev {:plugins [[lein-midje "3.2.1"]]}})


(defproject i-am-a-horse-in-the-land-of-booleans "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [iloveponies.tests/i-am-a-horse-in-the-land-of-booleans "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:plugins [[lein-midje "3.1.1"]]}})

