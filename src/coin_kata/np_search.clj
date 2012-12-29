(ns coin-kata.np-search)

; in repl: (use :reload 'coin-kata.np-search)

(defn init-change [denoms] (zipmap denoms (repeat 0)))

(defn init-possible [denoms] (list [0 0 (init-change denoms)]))

(defn greatest-possible [[x] [y]] (> x y))

(defn possible? [[a n c] amt] (<= a amt))

(defn expand-possible
  [denoms amount possible]
  (apply sorted-set-by greatest-possible
    (filter #(possible? % amount)
      (for [[a n c] possible d denoms]
        (let [new-a (+ d a)
              new-n (inc n)
              new-c (assoc c d (inc (get c d)))]
          [new-a new-n new-c])))))

(defn make-change
  "breadth first search"
  ([denoms amount]
    (loop [p (init-possible denoms)]
      (let [first-p (first p)]
        (if (= amount (first first-p))
          (nth first-p 2)
          (recur (expand-possible denoms amount p)))))))

