(ns aoc-2011.day-25-test 
  (:require [aoc-2011.day-25 :refer [a]]
            [clojure.test :refer [deftest is testing]]))

(def input "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>")

(deftest seat-cucumbers-test
      (testing "Day 25 - Part 1"
        (is (= 58 (a input))))) 