(ns coin-kata.np-lcm-greedy-reduce
  (:require [clojure.math.numeric-tower :as math]))

; in repl: (use :reload 'coin-kata.np-lcm-greedy-reduce)

(defn init-change [denoms] (zipmap denoms (repeat 0)))

(defn expand-denoms [denoms] (reverse (map reverse (drop 1 (reductions conj [] (sort < denoms))))))

(defn chgfn
  [[amount change] denoms]
  (let [lcm (reduce math/lcm 1 denoms)
        d (first denoms)
        n (* (/ lcm d) (quot amount lcm))]
    [(rem amount lcm) (assoc change d n)]))

(defn make-change
  "lcm greedy with reduce"
  [denoms amount]
  (second (reduce chgfn [amount (init-change denoms)] (expand-denoms denoms))))

