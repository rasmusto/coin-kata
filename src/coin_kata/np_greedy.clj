(ns coin-kata.np-greedy)

; in repl: (use :reload 'coin-kata.np-greedy)

(defn init-change [denoms] (zipmap denoms (repeat 0)))

(defn apply-denom
  [denom amount change]
  (let [next-amount (rem amount denom)
        next-change (assoc change denom (quot amount denom))]
    [next-amount next-change]))

(defn make-change
  "recursive greedy"
  ([denoms amount]
    (let [change (init-change denoms)]
      (loop [[d & ds] (sort > denoms)
              a amount
              c change]
        (cond
          (= 0 a) c
          (nil? d) nil
          :else
            (let [[next-a next-c] (apply-denom d a c)]
              (recur ds next-a next-c)))))))

