(ns coin-kata.nopolabs)
  
; in repl: (use :reload 'coin-kata.nopolabs)

(def us-coins [1 5 10 25])
(def x-coins [1 10 20 25])

(defn init-change [denoms] (zipmap denoms (repeat 0)))
(defn coin-count [change] (apply + (vals change)))
(defn expand-denoms [denoms] (reverse (map reverse (drop 1 (reductions conj [] denoms)))))
(defn least-coins [solutions] (first (sort-by coin-count solutions)))
(defn use-denom [denom amount] [(quot amount denom) (rem amount denom)])
(defn apply-denom
  [denom amount change]
  (let [[n-denom next-amount] (use-denom denom amount)
         next-change (assoc change denom n-denom)]
    [next-amount next-change]))

(defn make-change
  ([denoms amount]
    (let [change (init-change denoms)]
      (make-change (sort > denoms) amount change)))
  ([[denom & next-denoms] amount change]
    (cond
      (nil? denom) nil
      (= 0 amount) change
      :else
        (let [[next-amount next-change] (apply-denom denom amount change)]
          (make-change next-denoms next-amount next-change)))))

(defn mc2
  ([denoms amount]
    (let [change (init-change denoms)
          solutions (for [ds (expand-denoms denoms)] (mc2 ds amount change))]
      (least-coins solutions)))
  ([[denom & next-denoms] amount change]
    (cond
      (= 0 amount) change
      (nil? denom) nil
      :else
        (let [[next-amount next-change] (apply-denom denom amount change)]
          (mc2 next-denoms next-amount next-change)))))

(defn mc3
  ([denoms amount]
    (let [change (init-change denoms)
          solutions (map #(mc3 % amount change) (expand-denoms denoms))]
      (least-coins solutions)))
  ([[denom & next-denoms] amount change]
    (cond
      (= 0 amount) change
      (nil? denom) nil
      :else
        (let [[next-amount next-change] (apply-denom denom amount change)]
          (mc3 next-denoms next-amount next-change)))))

(defn possible? [{ a :amt } amt] (<= a amt))

(defn correct-change? [{ a :amt } amt] (= a amt))

(defn greatest-possible
  [x y]
  (let [x-amt (:amt x)
        y-amt (:amt y)]
    (> x-amt y-amt)))

(defn expand-possible
  [denoms amount possible]
  (apply sorted-set-by greatest-possible
    (filter #(possible? % amount)
      (for [{ a :amt n :n c :change} possible d denoms]
        (let [new-a (+ d a)
              new-n (inc n)
              new-c (assoc c d (inc (get c d)))]
          { :amt new-a :n new-n :change new-c })))))

(defn init-possible [denoms] (list { :amt 0 :n 0 :change (init-change denoms) }))

(defn mc5
  ([denoms amount]
    (mc5 denoms amount (init-possible denoms)))
  ([denoms amount possible]
    (if (= amount (:amt (first possible)))
      (:change (first possible))
      (mc5 denoms amount (expand-possible denoms amount possible)))))

(defn mc5r
  ([denoms amount]
    (loop [d denoms a amount p (init-possible denoms)]
      (prn (count p))
      (if (= a (:amt (first p)))
        (:change (first p))
        (recur d a (expand-possible d a p))))))

(defn greedy-changer
  [denom]
  (fn [limit]
    (let [n (quot limit denom)
          amount (* n denom)
          remaining (rem limit denom)]
      { :denom denom :count n :amount amount :remaining remaining })))
          
(defn mc
  "returns a function that makes change using given denoms"
  [denoms]
  (let [sorted-denoms (sort > denoms)
        changers (zipmap sorted-denoms (map greedy-changer sorted-denoms))]
    (fn [amount]
      (loop [a amount
             [d & ds] sorted-denoms
             change (init-change sorted-denoms)]
        (if (= a 0) change
          (let [ c ((get changers d) a)
                next-a (:remaining c)
                next-change (assoc change d (:count c))]
            (recur next-a ds next-change)))))))

(def x-chg (mc x-coins))

(def us-chg (mc us-coins))

