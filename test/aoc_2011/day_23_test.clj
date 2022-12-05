(ns aoc-2011.day-23-test 
  (:require [aoc-2011.day-23 :refer [a b]]
            [clojure.test :refer [deftest is testing]]))

(def input-a "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")

(def input-b "#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########")

(deftest amphipod-a-test
  (testing "Day 23 - Part 1"
    (is (= 12521 (a input-a)))))

(deftest amphipod-b-test
  (testing "Day 23 - Part 2"
    (is (= 44169 (b input-b)))))
