(ns aoc-2011.day-06-test
  (:require [aoc-2011.day-06 :refer :all])
  (:require [clojure.test :refer :all]))

(def input "3,4,3,1,2")

(deftest test-a
  (testing "Problem 6 Part 1"
    (is (= 5934 (a input)))))

(deftest test-b
  (testing "Problem 6 Part 2"
    (is (= 26984457539 (b input)))))
