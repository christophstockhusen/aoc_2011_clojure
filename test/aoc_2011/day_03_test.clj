(ns aoc-2011.day-03-test
  (:require [clojure.test :refer :all]
            [aoc-2011.day-03 :refer :all]
            [clojure.string :as str]))

(def input ["00100"
            "11110"
            "10110"
            "10111"
            "10101"
            "01111"
            "00111"
            "11100"
            "10000"
            "11001"
            "00010"
            "01010"])

(def output ["11110"
             "10110"
             "10111"
             "10101"
             "11100"
             "10000"
             "11001"])

(deftest test-filter-list
  (testing "Test filter list"
    (let [input (map #(str/split % #"") input)
          output (map #(str/split % #"") output)]
      (is (= output (filter-list input :most-common 0))))))

(deftest test-find-value-most-common
  (testing "Find with bit criteria most common"
    (let [input (map #(str/split % #"") input)
          output (str/split "10111" #"")]
      (is (= output (find-value input :most-common))))))

(deftest test-find-value-least-common
  (testing "Find with bit criteria least common"
    (let [input (map #(str/split % #"") input)
          output (str/split "01010" #"")]
      (is (= output (find-value input :least-common))))))
