(ns aoc-2011.day-13-test 
  (:require [aoc-2011.day-13 :refer :all])
  (:require [clojure.test :refer [deftest is testing]]))

(def input "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(deftest sample-input
  (testing "Day 13 Part 1"
    (is (= 17 (a input)))))
