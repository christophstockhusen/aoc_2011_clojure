(ns aoc-2011.day-09-test
  (:require [aoc-2011.day-10 :refer :all])
  (:require [clojure.test :refer :all]))

(def input "2199943210
3987894921
9856789892
8767896789
9899965678")

(deftest test-a
  (testing "Problem 9 Part 1"
    (is (= 15 (a input)))))

(deftest test-b
  (testing "Problem 9 Part 2"
    (is (= 1134 (b input)))))
