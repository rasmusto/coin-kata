(ns coin-kata.rasmusto)

(def denoms [1 5 10 25])

(defn total [coin-map]
  (reduce + (map * (keys coin-map) (vals coin-map))))

(defn assoc-or-inc [map key]
  (if (contains? map key)
    (assoc map key (inc (map key)))
    (assoc map key 1)))

(defn calc-count
  ([denoms amt] (calc-count denoms amt (zipmap denoms (cycle [0]))))
  ([denoms amt choices]
   (prn denoms amt choices)
   (if (seq denoms)
     (let [biggest-coin (apply max denoms)]
       (cond
         (> biggest-coin amt)
         (calc-count (remove (partial = biggest-coin) denoms) amt choices)
         :else
         (calc-count denoms (- amt biggest-coin) (assoc-or-inc choices biggest-coin))))
     choices)))

(comment
(identity denoms)
(total {1 2, 5 3})
(assoc-or-inc {1 1 2 2} 2)
(assoc-or-inc {1 1 2 2} 3)
(calc-count denoms 20)
(calc-count denoms 44)
(total (calc-count [1 5 10 25] 114))

(calc-count #{5 20 25} 80)

(do
(require '[clojure.test :as ctest])
(require '[coin-kata.core-test :as ct])
(binding [ct/coin-f calc-count]
  (ctest/run-tests 'coin-kata.core-test)))
)
