(ns coin-kata.nopolabs
  (:require [clojure.math.numeric-tower :as math]))

;; in repl: (use :reload 'coin-kata.nopolabs)

(def x-coins [1 10 20 25])
(def us-coins [1 5 10 25])

;; change is a map from denoms to quantities
(defn init-change [denoms] (zipmap denoms (repeat 0)))

(defn coin-count [change] (reduce + (vals change)))

(defn add-change [x y] (into {} (map (fn [k] [k (+ (get x k) (get y k 0))]) (keys x))))

(defn add-coins [change denom n] (assoc change denom (+ (get change denom) n)))
  
(defn add-coin [change denom] (add-coins change denom 1))
  
;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Depth first search

(defn depth-first-change
  [denoms amount]
  (letfn
    [(best-change [left right]
       (if (some nil? [left right])
         (or left right)
           (if (< (coin-count left) (coin-count right))
             left right)))

     (look-deeper [denoms amount change]
       (let [d (first denoms)
             left (search denoms (- amount d) (add-coin change d))
             right (search (next denoms) amount change)]
         (best-change left right)))

     (search [denoms amount change]
       (cond
         (zero? amount) change
         (< amount 0) nil
         (empty? denoms) nil
         :else (look-deeper denoms amount change)))]

    (search (sort > denoms) amount (init-change denoms))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Breadth first search

(defn breadth-first-change
  [denoms amount]
  (letfn
    [(init-possible [denoms] (list [0 (init-change denoms)]))

     (greatest-possible [[x-amt] [y-amt]] (> x-amt y-amt))

     (possible? [[amt chg] max-amount] (<= amt max-amount))

     (new-possible [[amt chg] d] [(+ d amt) (add-coin chg d)])

     (next-possible [possible denoms amount]
       (apply sorted-set-by greatest-possible
         (filter #(possible? % amount)
           (for [p possible d denoms]
             (new-possible p d)))))]

    (loop [possible (init-possible denoms)]
      (let [first-possible (first possible)]
        (if (= amount (first first-possible))
          (second first-possible)
          (recur (next-possible possible denoms amount)))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LCM optimization

(defn lcm-opt
  [chgfn]
  (fn [denoms amount]
    (let [ds (sort > denoms)
          lcm (reduce math/lcm 1 ds)]
      (if (< amount (* 2 lcm))
        (chgfn ds amount)
        (let [chg-amt (+ (rem amount lcm) lcm)
              chg (chgfn ds chg-amt)
              lcm-amt (- amount chg-amt)
              d (first ds)
              n (/ lcm-amt d)]
          (add-coins chg d n))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 

(def df-opt (lcm-opt depth-first-change))
(def bf-opt (lcm-opt breadth-first-change))

(def df-x (partial depth-first-change x-coins))
(def bf-x (partial breadth-first-change x-coins))
(def df-opt-x (partial df-opt x-coins))
(def bf-opt-x (partial bf-opt x-coins))

(defn test-x [n]
  (filter (complement nil?)
    (for [i (range 1 (inc n))]
      (let [nd (coin-count (df-x i))
            nb (coin-count (bf-x i))
            ndo (coin-count (df-opt-x i))
            nbo (coin-count (bf-opt-x i))]
        (if (= nd nb ndo nbo)
          nil
          i)))))

