(ns coin-kata.np
  (:require [coin-kata.np-greedy :as greedy])
  (:require [coin-kata.np-lcm-greedy :as lcm-greedy])
  (:require [coin-kata.np-lcm-greedy-reduce :as lcm-greedy-reduce])
  (:require [coin-kata.np-search :as search]))

; in repl: (use :reload 'coin-kata.np)

(def us-coins [1 5 10 25])
(def x-coins [1 10 20 25])

(defn greedy [amt] (greedy/make-change x-coins amt))
(defn lcm-greedy [amt] (lcm-greedy/make-change x-coins amt))
(defn lcm-greedy-reduce [amt] (lcm-greedy-reduce/make-change x-coins amt))
(defn search [amt] (search x-coins amt))

