(ns aoc-2011.day-12-test
  (:require [aoc-2011.day-12 :refer :all])
  (:require [clojure.test :refer :all]))

(def small-input "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def medium-input "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(def large-input "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(deftest test-a-small
  (testing "Problem 12 Part 1 (Small Example)"
    (is (= 10 (a small-input)))))

(deftest test-a-medium
  (testing "Problem 12 Part 1 (Medium Example)"
    (is (= 19 (a medium-input)))))

(deftest test-a-large
  (testing "Problem 12 Part 1 (Large Example)"
    (is (= 226 (a large-input)))))

(deftest test-b-small
  (testing "Problem 12 Part 2 (Small Example)"
    (is (= 36 (b small-input)))))
