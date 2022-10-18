(ns aoc-2011.day-13
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [join replace-first split split-lines]]))

(defn- parse-coordinates [input]
  (->> (split-lines input)
       (map #(split % #","))
       (map #(map parse-long %))
       (set)))

(defn- parse-fold [input]
  (as-> (replace-first input #"fold along " "") v
    (split v #"=")
    (if (= "x" (first v))
      {:axis :horizontal :offset (parse-long (second v))}
      {:axis :vertical :offset (parse-long (second v))})))

(defn- parse-folds [input]
  (->> (split-lines input)
       (map parse-fold)))

(defn- parse-input [input]
  (let [parts (split input #"\n\n")
        dots (parse-coordinates (first parts))
        folds (parse-folds (second parts))
        max-x (apply max (map first dots))
        max-y (apply max (map second dots))]
    {:dots dots :folds folds :max-x max-x :max-y max-y}))

(defn- fold-component [size c offset]
  (let [shift (max 0 (- size (* 2 offset)))]
    (if (< c offset)
      (+ shift c)
      (+ shift (- c (* 2 (- c offset)))))))

(defn- fold-dot [[x y] [max-x max-y] {offset :offset axis :axis}]
  (if (= axis :horizontal)
    [(fold-component max-x x offset) y]
    [x (fold-component max-y y offset)]))

(defn- fold [{dots :dots max-x :max-x max-y :max-y} {offset :offset axis :axis :as f}]
  (let [folded-dots (set (map #(fold-dot % [max-x max-y] f) dots))
        new-max-x (if (= axis :horizontal) (dec offset) max-x)
        new-max-y (if (= axis :vertical) (dec offset) max-y)]
    {:dots folded-dots :max-x new-max-x :max-y new-max-y}))

(defn- process-folds [dots folds]
  (reduce fold dots folds))

(defn a
  ([] (a (slurp (resource "13.txt"))))
  ([input]
   (let [{dots :dots folds :folds max-x :max-x max-y :max-y} (parse-input input)]
     (->> (process-folds {:dots dots :max-x max-x :max-y max-y} (take 1 folds))
          (:dots)
          (count)))))

(defn- print-dots [{dots :dots max-x :max-x max-y :max-y}]
  (->> (range (inc max-y))
       (map (fn [row] (map #(vector % row) (range (inc max-x)))))
       (map (fn [row] (map #(if (contains? dots %) "#" " ") row)))
       (map join)
       (join "\n")
       (print)))

(defn b
  ([] (b (slurp (resource "13.txt"))))
  ([input]
   (let [{dots :dots folds :folds max-x :max-x max-y :max-y} (parse-input input)]
     (->> (process-folds {:dots dots :max-x max-x :max-y max-y} folds)
          (print-dots)))))
