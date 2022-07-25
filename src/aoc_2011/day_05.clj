(ns aoc-2011.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-coordinate
  [c]
  {:x (Integer/parseInt (first c)) :y (Integer/parseInt (second c))})

(defn parse-line
  [s]
  (let [splitted (map #(str/split % #",") (str/split s #" -> "))]
    [(parse-coordinate (first splitted))
     (parse-coordinate (second splitted))]))

(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (map parse-line)))

(defn one-common-coordinate?
  [[a b]]
  (or (= (:x a) (:x b))
      (= (:y a) (:y b))))

(defn nat-from-to
  [a b]
  (if (<= a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn line-coordinates
  [[a b]]
  (let [xs (if (= (:x a) (:x b))
             (repeat (:x a))
             (nat-from-to (:x a) (:x b)))
        ys (if (= (:y a) (:y b))
             (repeat (:y a))
             (nat-from-to (:y a) (:y b)))]
    (map #(hash-map :x %1 :y %2) xs ys)))

(defn inc-coordinate
  [coordinates coordinate]
  (update coordinates coordinate #(if (some? %) (inc %) 1)))

(defn compute-covered-coordinates
  [line-coordinates]
  (reduce inc-coordinate {} line-coordinates))

(defn a
  ([] (a (slurp (io/resource "05.txt"))))
  ([input]
   (->>
    (parse-input input)
    (filter one-common-coordinate?)
    (map line-coordinates)
    (flatten)
    (compute-covered-coordinates)
    (filter (fn [[_ v]] (> v 1)))
    (count))))

(defn b
  ([] (b (slurp (io/resource "05.txt"))))
  ([input]
   (->>
    (parse-input input)
    (map line-coordinates)
    (flatten)
    (compute-covered-coordinates)
    (filter (fn [[_ v]] (> v 1)))
    (count))))
