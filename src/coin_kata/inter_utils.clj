(ns coin-kata.inter-utils
  (:require [clojure.tools.namespace.repl :as trepl]))

;; utils for interactive development
(comment
  ;; Refresh the entire classpath, pulling in new deps if needed and checking the dependency order
  (trepl/refresh))

;; short hand to run tests via `cpp` instead of `:make tests`
(comment
  (do
    (require '[clojure.test :as ctest])
    (require '[coin-kata.core-test :as ct])
    (binding [ct/coin-f greedy-reduce]
      (ctest/run-tests 'coin-kata.core-test))))

