(ns aoc-2011.day-07-test
  (:require [aoc-2011.day-07 :refer :all])
  (:require [clojure.test :refer :all]))

(def input "16,1,2,0,4,2,7,1,2,14")

(deftest test-a
  (testing "Problem 7 Part 1"
    (is (= 37 (a input)))))

(deftest test-b
  (testing "Problem 7 Part 2"
    (is (= 168 (b input)))))
