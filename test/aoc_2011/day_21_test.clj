(ns aoc-2011.day-21-test
  (:require [aoc-2011.day-21 :refer [a b]]
            [clojure.test :refer [deftest is testing]]))

(def input "Player 1 starting position: 4
Player 2 starting position: 8")

(deftest dirac-dice-a-test
  (testing "Day 21 - Part 1"
    (is (= 739785 (a input)))))

(deftest dirac-dice-b-test
  (testing "Day 21 - Part 2"
    (is (= 444356092776315 (b input)))))