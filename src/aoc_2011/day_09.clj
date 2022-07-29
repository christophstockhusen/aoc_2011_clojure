(ns aoc-2011.day-09
  (:require [clojure.java.io :as io]
            [clojure.set :refer [difference]]
            [clojure.string :as str :refer [split-lines]]))

(defn- parse-line
  [line]
  (->> (str/split line #"")
       (map #(Integer/parseInt %))))

(defn- parse-input
  [input]
  (->> input
       (split-lines)
       (map parse-line)))

(defn- m-get
  [m [x y]]
  (nth (nth m y nil) x 10))

(defn- above [m [x y]] (m-get m [x (dec y)]))

(defn- below [m [x y]] (m-get m [x (inc y)]))

(defn- left [m [x y]] (m-get m [(dec x) y]))

(defn- right [m [x y]] (m-get m [(inc x) y]))

(defn- is-low-point?
  [m [x y]]
  (let [h (nth (nth m y) x)]
    (and (< h (above m [x y]))
         (< h (below m [x y]))
         (< h (left m [x y]))
         (< h (right m [x y])))))

(defn- filter-low-points
  [matrix]
  (let [width (count (first matrix))
        height (count matrix)
        coordinates (for [x (range width) y (range height)] (vector x y))]
    (mapcat (fn [[x y]] (if (is-low-point? matrix [x y])
                          [{:value (nth (nth matrix y) x) :coordinates [x y]}]
                          []))
            coordinates)))

(defn a
  ([] (a (slurp (io/resource "09.txt"))))
  ([input]
   (->> (parse-input input)
        (filter-low-points)
        (map :value)
        (map inc)
        (apply +))))

(defn- neighbor-coordinates
  [[x y]]
  [[x (dec y)]
   [(inc x) y]
   [x (inc y)]
   [(dec x) y]])

(defn- neighbors
  [matrix p]
  (->> (neighbor-coordinates p)
       (map (fn [x] {:value (m-get matrix x) :coordinates x}))
       (filter #(some? (:value %)))
       (filter #(< (:value %) 9))
       (map #(:coordinates %))))

(defn- compute-basin-size
  [matrix p]
  (loop [current p
         visited #{}
         q clojure.lang.PersistentQueue/EMPTY]
    (let [q (apply conj q (difference (set (neighbors matrix current))
                                      visited
                                      #{current}))]
      (if (not (some? (peek q)))
        (count (conj visited current))
        (recur (peek q)
               (conj visited current)
               (pop q))))))

(defn- compute-basin-sizes
  [matrix low-points]
  (map (partial compute-basin-size matrix) low-points))

(defn- find-largest-basins
  [matrix]
  (->> matrix
       (filter-low-points)
       (map :coordinates)
       (compute-basin-sizes matrix)
       (sort >)))

(defn b
  ([] (b (slurp (io/resource "09.txt"))))
  ([input]
   (->> (parse-input input)
        (find-largest-basins)
        (take 3)
        (apply *))))
