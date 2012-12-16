(ns coin-kata.inter-utils
  (:require [clojure.tools.namespace.repl :as trepl]))

;; utils for interactive development
(comment
  ;; Refresh the entire classpath, pulling in new deps if needed and checking the dependency order
  (trepl/refresh))

