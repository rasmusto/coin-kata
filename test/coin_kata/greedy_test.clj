(ns coin-kata.greedy-test
  (:require [clojure.test :as ctest :refer [deftest with-test testing are]]
            [coin-kata.greedy :as greedy]))

(def ^:dynamic coin-f greedy/greedy-recur)

(deftest greedy
  (testing "Sub-dollar amounts"
    (testing "with #{1 5 10 25}"
      (let [denominations #{1 5 10 25}]
        (are [actual expected] (= actual expected)
             (coin-f denominations 55) {1 0, 5 1, 10 0, 25 2}
             (coin-f denominations 80) {1 0, 5 1, 10 0, 25 3}
             (coin-f denominations 0)  {1 0, 5 0, 10 0, 25 0})))
    (testing "with #{1 20 25}"
      (let [denominations #{1 20 25}]
        (are [actual expected] (= actual expected)
             (coin-f denominations 55) {1 5, 20 0, 25 2}
             (coin-f denominations 80) {1 0, 20 4, 25 0}
             (coin-f denominations 0)  {1 0, 20 0, 25 0})))
     (testing "with #{1 20 25 75}"
      (let [denominations #{1 20 25 75}]
        (are [actual expected] (= actual expected)
             (coin-f denominations 55) {1 5, 20 0, 25 2, 75 0}
             (coin-f denominations 80) {1 0, 20 4, 25 0, 75 0}
             (coin-f denominations 0)  {1 0, 20 0, 25 0, 75 0})))))

