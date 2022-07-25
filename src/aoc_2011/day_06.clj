(ns aoc-2011.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- parse-input [input]
  (->>
   (str/split input #",")
   (map #(Integer/parseInt %))))

(defn- step
  [counts]
  (let [decreased (->> counts
                       (map (fn [[k v]] [(dec k) v]))
                       (flatten)
                       (apply hash-map))]
    (-> decreased
        (update 6 #(+ (or % 0) (get decreased -1 0)))
        (assoc 8 (get decreased -1 0))
        (dissoc -1))))

(defn- simulate-steps
  [input steps]
  (let [simulation (->>
                    (parse-input input)
                    (frequencies)
                    (iterate step))]
    (apply + (vals (nth simulation steps)))))

(defn a
  ([] (a (slurp (io/resource "06.txt"))))
  ([input] (simulate-steps input 80)))

(defn b
  ([] (b (slurp (io/resource "06.txt"))))
  ([input] (simulate-steps input 256)))
