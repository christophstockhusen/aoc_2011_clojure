(ns aoc-2011.day-11-test
  (:require [aoc-2011.day-11 :refer :all])
  (:require [clojure.test :refer :all]))

(def input "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(deftest test-a
  (testing "Problem 11 Part 1"
    (is (= 1656 (a input)))))

(deftest test-b
  (testing "Problem 11 Part 2"
    (is (= 195 (b input)))))
