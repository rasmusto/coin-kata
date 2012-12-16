(ns coin-kata-pdg.coin-shift
  (:require [clojure.core.reducers :as r]))

(defn make-change
  ""
  [denominations amount]
  (cond
    (or (neg? amount)
        (empty? denominations)) nil
    (zero? amount) (zipmap denominations (repeat 0))
    :else (let [options (keep identity (for [coin denominations]
                            (when-let [sol (make-change denominations (- amount coin))]
                              (update-in sol [coin] inc))))]
            (apply min-key #(apply + (vals %)) options))))
(def make-change-memo (memoize make-change))

(defn make-change2
  ""
  [denominations amount]
  (cond
    (or (neg? amount)
        (empty? denominations)) nil
    (zero? amount) (zipmap denominations (repeat 0))
    :else (let [options (into []
                          (r/filter identity 
                            (r/map (fn [coin]
                                     (when-let [sol (make-change2 denominations (- amount coin))]
                                       (update-in sol [coin] inc))) denominations)))]
            (apply min-key #(apply + (vals %)) options))))
(def make-change2-memo (memoize make-change2))

(comment
  (make-change2 [1 20 25] 80)
  (do
    (require '[clojure.test :as ctest])
    (require '[coin-kata.core-test :as ct])
    (binding [ct/coin-f make-change2-memo]
      (ctest/run-tests 'coin-kata.core-test))))

