(ns coin-kata-pdg.greedy-drop
  (:require [coin-kata-pdg.greedy :as greedy]))

(defn greedy-drop
  ""
  [denoms amount]
  (let [denominations (apply sorted-set denoms)]
    (merge (zipmap denominations (repeat 0))
           (apply min-key
                  #(apply + (vals %))
                  (map #(greedy/greedy-recur % amount) (nnext (reductions conj [] denominations)))))))

(comment
  
  (next [])
  (take-while identity (iterate next [1 2 3]))
  (nnext (reductions conj [] [1 2 3]))

  (greedy-drop [1 20 25] 80)

  (do
    (require '[clojure.test :as ctest])
    (require '[coin-kata.core-test :as ct])
    (binding [ct/coin-f greedy-drop]
      (ctest/run-tests 'coin-kata.core-test)))
  )
