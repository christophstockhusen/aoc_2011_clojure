(ns aoc-2011.day-02
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn to-vector
  [[direction value]]
  (let [v (Integer/parseInt value)]
    (case direction
      "forward" [v 0]
      "down" [0 v]
      "up" [0 (- v)])))

(defn a
  []
  (->>
   (slurp (io/resource "02.txt"))
   (str/split-lines)
   (map #(str/split % #" "))
   (map to-vector)
   (apply mapv +)
   (apply *)))

(defn combine
  [[h-pos depth aim] [direction x]]
  (case direction
    "forward" [(+ h-pos x) (+ depth (* aim x)) aim]
    "down" [h-pos depth (+ aim x)]
    "up" [h-pos depth (- aim x)]))

(defn b
  []
 (->>
  (slurp (io/resource "02.txt"))
  (str/split-lines)
  (map #(str/split % #" "))
  (map (fn [[d x]] [d (Integer/parseInt x)]))
  (reduce combine [0 0 0])
  (take 2)
  (apply *)))
