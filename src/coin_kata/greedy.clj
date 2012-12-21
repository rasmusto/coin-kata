(ns coin-kata.greedy
  (:require [clojure.core.reducers :as r]))


(defn cost [sol-map]
  (apply + (vals sol-map)))

(defn calculate [sol-map-amount-vec denomination] ; => [updated-solution-map updated-r-amount]
  (let [[sol-map r-amount] sol-map-amount-vec
        coins-for-denomination (quot r-amount denomination)
        value-of-those-coins (* coins-for-denomination denomination)
        new-r-amount (- r-amount value-of-those-coins)
        new-sol-map (assoc sol-map denomination coins-for-denomination)]
    [new-sol-map new-r-amount]))

(defn make-change [denominations amount]
  (let [denoms (reverse (apply sorted-set denominations))
        sol-map (zipmap denoms (repeat 0))
        sol-maps (r/map #(first (reduce calculate [sol-map amount] %)) (drop 2 (reductions conj [] denoms)))]
   (apply min-key cost sol-maps)))


(comment 
 (cost {1 5, 5 3})
  (min-key)
  (drop 2 (reductions conj [] [1 5 10 25]))
  (def denominations [1 5 10 25])
(seq? (reverse (apply sorted-set denominations)))
(list? (reverse (apply sorted-set denominations)))

  (make-change [1 5 10 25] 18)
(do
    (require '[clojure.test :as ctest])
    (require '[coin-kata.core-test :as ct])
    (binding [ct/coin-f make-change]
      (ctest/run-tests 'coin-kata.core-test)))
  )
