(ns aoc-2011.day-07 
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input
  [input] (map #(Integer/parseInt %) (str/split input #",")))

(defn distances
  [p positions]
  (map #(abs (- p %)) positions))

(defn dist-a [x] x)

(defn fuel
  [positions dist-f]
  (let [
        min-pos (apply min positions)
        max-pos (apply max positions)]
    (->> (range min-pos (inc max-pos))
         (map #(distances % positions))
         (map (fn [ds] (map dist-f ds)))
         (map #(apply + %))
         (apply min))))

(defn a
  ([] (a (slurp (io/resource "07.txt"))))
  ([input] 
   (fuel (parse-input input) dist-a)))

(defn dist-b [x] (/ (* x (inc x)) 2))

(defn b
  ([] (b (slurp (io/resource "07.txt"))))
  ([input]
   (fuel (parse-input input) dist-b)))

(b)