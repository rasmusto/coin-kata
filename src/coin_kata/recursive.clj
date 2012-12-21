(ns coin-kata.recursive)

;; This solution came in from Greg
;; Some things were cleaned up by Paul

;; Here's the original code
(defn maybeCons [x xs]
  (if (= xs nil) nil (cons x xs)))

(defn best_of [left right]
  (cond (= right nil) left
        (= left nil) right
        (> (count right)(count left)) left
        true right))

(defn kata [coins amt]
  (cond (= amt 0) []
        (empty? coins) nil
        (< amt (first coins)) (kata (drop 1 coins) amt)
        true (let [left (maybeCons (first coins)(kata coins (- amt (first coins))))
                   right (kata (drop 1 coins) amt)]
               (best_of left right))))

;; Small cleanup by Paul - not tested

;; A small tip here - typically in clojure, you let nils pile up,
;; and filter them out later with filter or remove.
;;
;; When you want to handle a Maybe-style case, you usually wrap
;; the function with (fnil ...)
;; What we're doing here is a little different (short-circuiting)
(defn nil-conj [xs x]
  (when-not (nil? x)
    (conj xs x)))

(defn best_of [left right]
  (if (some nil? [left right])
    (or left right)
    (min-key count left right)))

(defn kata [coins amt]
  (cond (zero? amt) []
        (empty? coins) nil
        (< amt (first coins)) (kata (next coins) amt)
        :else (let [left (nil-conj (kata coins (- amt (first coins)))
                                   (first coins))
                   right (kata (next coins) amt)]
               (best_of left right))))

;; to get the map-based answer you just
;; (zipmap coins (kata coins amt))

