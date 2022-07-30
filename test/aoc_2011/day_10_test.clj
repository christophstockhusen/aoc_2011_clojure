(ns aoc-2011.day-10-test
  (:require [aoc-2011.day-10 :refer :all])
  (:require [clojure.test :refer :all]))

(def input "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(deftest test-a
  (testing "Problem 10 Part 1"
    (is (= 26397 (a input)))))

(deftest test-b
  (testing "Problem 10 Part 2"
    (is (= 288957 (b input)))))
