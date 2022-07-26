(ns aoc-2011.day-08
  (:require [clojure.java.io :as io]
            [clojure.set :as set :refer [intersection map-invert]]
            [clojure.string :as str :refer [join]]))

(defn- parse-line
  [line]
  (->> (str/split line #"\|")
       (map str/trim)
       (map #(str/split % #" "))
       (map #(map set %))))

(defn- parse-input
  [input]
  (->> input
       (str/split-lines)
       (map parse-line)))

(defn- is-1478?
  [s]
  (contains? #{2 4 3 7} (count s)))

(defn a
  ([] (a (slurp (io/resource "08.txt"))))
  ([input]
   (->> (parse-input input)
        (map #(nth % 1))
        (map #(filter is-1478? %))
        (flatten)
        (count))))

;; We can deduce the encoded digits as follows:
;;
;; First, we identify digits 1, 4, 7, 8 by their unique number
;; active segments (2, 4, 3, 7).
;;
;; Then, we can deduce the remaining digits by considering the
;; number of their active segments and their overlaps with other
;; digits.

(defn- is-1? [s] (= 2 (count s)))

(defn- is-4? [s] (= 4 (count s)))

(defn- is-7? [s] (= 3 (count s)))

(defn- is-8? [s] (= 7 (count s)))

(defn- is-2? [s segments-4 segments-7]
  (and (= 5 (count s))
       (= 2 (count (intersection s segments-4)))
       (= 2 (count (intersection s segments-7)))))

(defn- is-3? [s segments-4 segments-7]
  (and (= 5 (count s))
       (= 3 (count (intersection s segments-4)))
       (= 3 (count (intersection s segments-7)))))

(defn- is-5? [s segments-4 segments-7]
  (and (= 5 (count s))
       (= 3 (count (intersection s segments-4)))
       (= 2 (count (intersection s segments-7)))))

(defn- is-0? [s segments-1 segments-4]
  (and (= 6 (count s))
       (= 2 (count (intersection s segments-1)))
       (= 3 (count (intersection s segments-4)))))

(defn- is-6? [s segments-1 segments-4]
  (and (= 6 (count s))
       (= 1 (count (intersection s segments-1)))
       (= 3 (count (intersection s segments-4)))))

(defn- is-9? [s segments-4]
  (and (= 6 (count s))
       (= 4 (count (intersection s segments-4)))))

(defn- deduce-1478
  [signals]
  {1 (first (filter is-1? signals))
   4 (first (filter is-4? signals))
   7 (first (filter is-7? signals))
   8 (first (filter is-8? signals))})

(defn- deduce-023569
  [signals segments-1478]
  {0 (first (filter #(is-0? % (get segments-1478 1) (get segments-1478 4)) signals))
   2 (first (filter #(is-2? % (get segments-1478 4) (get segments-1478 7)) signals))
   3 (first (filter #(is-3? % (get segments-1478 4) (get segments-1478 7)) signals))
   5 (first (filter #(is-5? % (get segments-1478 4) (get segments-1478 7)) signals))
   6 (first (filter #(is-6? % (get segments-1478 1) (get segments-1478 4)) signals))
   9 (first (filter #(is-9? % (get segments-1478 4)) signals))})

(defn- deduce-output-values
  [segments output-signals]
  (let [inverted (map-invert segments)]
    (map #(get inverted %) output-signals)))

(defn- deduce-row
  [row]
  (let [signals (nth row 0)
        segments-1478 (deduce-1478 signals)
        segments-023569 (deduce-023569 signals segments-1478)
        segments (merge segments-1478 segments-023569)]
    (deduce-output-values segments (nth row 1))))

(defn b
  ([] (b (slurp (io/resource "08.txt"))))
  ([input]
   (->> (parse-input input)
        (map deduce-row)
        (map join)
        (map #(Integer/parseInt %))
        (apply +))))
