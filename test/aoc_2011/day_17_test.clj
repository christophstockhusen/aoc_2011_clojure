(ns aoc-2011.day-17-test 
  (:require [aoc-2011.day-17 :refer [a b]]
            [clojure.test :refer [deftest is testing]]))

(def input "target area: x=20..30, y=-10..-5")

(deftest highest-y-position
  (testing "Day 17 - Part 1"
    (is (= 45 (a input)))))

(deftest distinct-initial-velocity-values
  (testing "Day 17 - Part 2"
    (is (= 112 (b input)))))
