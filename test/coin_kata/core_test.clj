(ns coin-kata.core-test
  (:require [clojure.test :as ctest :refer [deftest with-test testing are]]
            [coin-kata-pdg.greedy :as greedy]))

(def ^:dynamic coin-f greedy/greedy-recur)

(deftest kata-test
  (testing "Sub-dollar amounts"
    (testing "with #{1 5 10 25}"
      (let [denominations #{1 5 10 25}]
        (are [actual expected] (= (coin-f denominations actual) expected)
             55 {1 0, 5 1, 10 0, 25 2}
             80 {1 0, 5 1, 10 0, 25 3}
             0  {1 0, 5 0, 10 0, 25 0})))
    (testing "with #{1 20 25}"
      (let [denominations #{1 20 25}]
        (are [actual expected] (= (coin-f denominations actual) expected)
             55 {1 5, 20 0, 25 2}
             80 {1 0, 20 4, 25 0}
             0  {1 0, 20 0, 25 0})))
     (testing "with #{1 20 25 75}"
      (let [denominations #{1 20 25 75}]
        (are [actual expected] (= (coin-f denominations actual) expected)
             55 {1 5, 20 0, 25 2, 75 0}
             80 {1 0, 20 4, 25 0, 75 0}
             0  {1 0, 20 0, 25 0, 75 0}))))
  #_(testing "Dollar+ amounts"
    (testing "with #{1 5 10 25}"
      (let [denominations #{1 5 10 25}]
        (are [actual expected] (= (coin-f denominations actual) expected)
             100 {1 0, 5 0, 10 0, 25 4}
             280 {1 0, 5 1, 10 0, 25 11}
             141 {1 1, 5 1, 10 1, 25 5})))
    (testing "with #{1 20 25}"
      (let [denominations #{1 20 25}]
        (are [actual expected] (= (coin-f denominations actual) expected)
             100 {1 0, 20 0, 25 4}
             280 {1 0, 20 4, 25 8}
             131 {1 6, 20 0, 25 5})))
     (testing "with #{1 20 25 75}"
      (let [denominations #{1 20 25 75}]
        (are [actual expected] (= (coin-f denominations actual) expected)
             100 {1 0, 20 0, 25 1, 75 1}
             280 {1 0, 20 4, 25 2, 75 2}
             131 {1 6, 20 0, 25 2, 75 1}))))
  (testing "Non-single cent denominations")
  (testing "Impossible amounts"))

