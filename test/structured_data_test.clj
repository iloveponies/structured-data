(ns structured-data-test
  (:use iloveponies.tests.structured-data
        structured-data
        midje.sweet))

(facts "spiff-addendum"
  (spiff [1 2]) => (throws Exception)
  (spiff []) => (throws Exception))

;; assignment specifies vector as input
(facts "cutify-addendum"
  (cutify '(1 2)) => (throws Exception))