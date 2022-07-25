(ns aoc-2011.day-01
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))


(defn a
  []
  (let [x (->> (slurp (io/resource "01.txt"))
               (str/split-lines)
               (map #(Integer/parseInt %)))]
    (->>
     (map vector x (rest x))
     (map #(apply < %))
     (filter true?)
     (count))))

(defn b
  []
  (let [x (->> (slurp (io/resource "01.txt"))
               (str/split-lines)
               (map #(Integer/parseInt %))
               (partition 3 1)
               (map #(apply + %)))]
    (->>
     (map vector x (rest x))
     (map #(apply < %))
     (filter true?)
     (count))))
