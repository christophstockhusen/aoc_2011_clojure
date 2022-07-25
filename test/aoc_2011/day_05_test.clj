(ns aoc-2011.day-05-test
  (:require [aoc-2011.day-05 :refer :all])
  (:require [clojure.test :refer :all]))

(def input
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(deftest test-a
  (testing "Problem 5 Part 1"
    (is (= 5 (a input)))))

(deftest test-b
  (testing "Problem 5 Part 2"
    (is (= 12 (b input)))))

