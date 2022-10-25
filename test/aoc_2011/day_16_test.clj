(ns aoc-2011.day-16-test 
  (:require [aoc-2011.day-16 :refer [a b]]
            [clojure.test :refer [deftest is testing]]))

(deftest version-sums
  (testing "Day 15 Pt. 1 - version sums"
    (is (= 16 (a "8A004A801A8002F478")))
    (is (= 12 (a "620080001611562C8802118E34")))
    (is (= 23 (a "C0015000016115A2E0802F182340")))
    (is (= 31 (a "A0016C880162017C3686B18A3D4780")))))

(deftest package-evaluation
  (testing "Day 15 Pt. 2 - evaluation"
    (is (= 3 (b "C200B40A82")))
    (is (= 54 (b "04005AC33890")))
    (is (= 7 (b "880086C3E88112")))
    (is (= 9 (b "CE00C43D881120")))
    (is (= 1 (b "D8005AC2A8F0")))
    (is (= 0 (b "F600BC2D8F")))
    (is (= 0 (b "9C005AC2F8F0")))
    (is (= 1 (b "9C0141080250320F1802104A08")))))
