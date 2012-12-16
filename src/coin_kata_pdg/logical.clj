(ns coin-kata-pdg.logical
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.arithmetic :as a]))

;; The first step is describing (in natural language)
;; the problem, free of any programming bias.
;;
;; Constantly rework the wording until you're
;; explaining the logic solution within the problem definition
;;
;; Your goal is to setup the structure of the logic programming solution

;; Given an integer amount of change to return
;; And a vector of intege, the denominations of coinage,
;; Produce the amount of change, using the least possible coinage

(l/defc correct-amountc [amount denom-vec q]
  (= amount (apply + (map * denom-vec q))))

(l/defc least-coinsc [sol options]
  ((<= (apply + sol) (apply min (map #(apply + %) options)))))

(l/defc least-coinsc-s [sol options]
  (and (some #{sol} options)
       (<= (apply + sol) (apply min (map #(apply + %) options)))))

(comment
 (some #{[1 2]} [[3 4] [5 6] [1 2]]) 
  (l/run* [q]
    (l/fresh [quarters dimes nickles pennies x
              qq qd qn qp]
      (l/infd quarters dimes nickles pennies (l/domain 0 1 2 3 4 5 6 7 8 9 10))
      (l/infd qq qd qn qp (l/domain 0 1 2 3 4 5 6 7 8 9 10))
      (l/== x [quarters dimes nickles pennies])
      (correct-amountc 18 [25 10 5 1] x)
      ;(l/== q x)
      (l/== q [qq qd qn qp])
      ;(least-coinsc-s q x)
      (correct-amountc 18 [25 10 5 1] q)
      (least-coinsc q x)
      ))
  
  )

