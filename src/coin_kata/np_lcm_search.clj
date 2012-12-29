(ns coin-kata.np-lcm-search
  (:require [clojure.math.numeric-tower :as math]))

; in repl: (use :reload 'coin-kata.np-lcm-search)

; change is a map from denoms to quantities
(defn init-change [denoms] (zipmap denoms (repeat 0)))

; possible change is a vector of amount and change
(defn init-possible [denoms] (list [0 (init-change denoms)]))

(defn greatest-possible [[x] [y]] (> x y))

(defn possible? [[a c] amt] (<= a amt))

(defn new-possible [[a c] d] [(+ d a) (assoc c d (inc (get c d)))])

(defn expand-possible
  [denoms amount possible]
  (apply sorted-set-by greatest-possible
    (filter #(possible? % amount)
      (for [p possible d denoms]
        (new-possible p d)))))

(defn search-change
  "lcm search"
  [denoms amount]
  (loop [p (init-possible denoms)]
    (let [first-p (first p)]
      (if (= amount (first first-p))
        (second first-p)
        (recur (expand-possible denoms amount p))))))

(defn add-change
  "adds two change maps, assumes x has all available denomination keys, and y has a subset"
  [x y]
  (into {} (map (fn [k] [k (+ (get x k) (get y k 0))]) (keys x))))

(defn lcm-opt
  "Lcm optimization, makes change using the largest available denomination
   for portion of amount that is a multiple of the least common multiple
   of all available denominations.
   Accepts the available denominations and the amount of change needed.
   Returns the denomination used, the number used, and the remaining amount."
  [denoms amount]
  (let [lcm (reduce math/lcm 1 denoms)
        d (first denoms)
        n (* (/ lcm d) (quot amount lcm))
        r (rem amount lcm)]
    [d n r]))

(defn make-change
  "search using lcm optimization"
  [denoms amount]
  (loop [ds (sort > denoms)
         amt amount
         chg (init-change denoms)]
    (if (= 0 amt)
      ; done making change, return the result
      chg
      ; otherwise apply lcm optimization
      (let [[d n r] (lcm-opt ds amt)]
        (if (= 0 n)
          ; no lcm change, apply search to remaining amount and return the result
          (add-change chg (search-change ds amt))
          ; otherwise recurse using remaining denominations and amount
          (recur (rest ds) r (assoc chg d n)))))))

