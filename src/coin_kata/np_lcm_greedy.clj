 (ns coin-kata.np-lcm-greedy
  (:require [clojure.math.numeric-tower :as math]))

; in repl: (use :reload 'coin-kata.np-lcm-greedy)

(defn init-change [denoms] (zipmap denoms (repeat 0)))

(defn make-change
  "greedy using lcm optimization"
  [denoms amount]
  (loop [amt amount
         ds (sort > denoms)
         change (init-change denoms)]
    (if (= 0 amt) change
      (let [lcm (reduce math/lcm 1 ds)
            d (first ds)
            n (* (/ lcm d) (quot amt lcm))
            a (rem amt lcm)]
        (recur a (rest ds) (assoc change d n))))))

