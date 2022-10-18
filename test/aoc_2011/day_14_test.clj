(ns aoc-2011.day-14-test
  (:require [aoc-2011.day-14 :refer :all]
            [clojure.test :refer [deftest is testing]]))

(def input "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(deftest polymerization
  (testing "Day 14 Part 1"
    (is (= 1588 (a input)))))

(deftest polymerization
  (testing "Day 14 Part 2"
    (is (= 2188189693529 (b input)))))
