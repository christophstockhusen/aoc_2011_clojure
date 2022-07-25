(ns aoc-2011.day-03
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn add-bit
  [[x0 x1] x]
  (case x
    "0" [(inc x0) x1]
    "1" [x0 (inc x1)]))

(defn add-vec
  [x y]
  (map add-bit x y))

(defn to-bit-max
  [[x0 x1]]
  (if (> x0 x1)
    "0"
    "1"))

(defn invert
  [x]
  (str/join (map #(- 1 (Integer/parseInt (str %))) x)))

(defn a
  []
  (let [x (map #(str/split % #"") 
               (str/split-lines (slurp (io/resource "03.txt"))))]
    (->>
     (reduce add-vec (apply vector (repeat (count (first x)) [0 0])) x)
     (map to-bit-max)
     (str/join)
     ((fn [x] [x (invert x)]))
     (map #(Long/parseLong % 2))
     (apply *))))

(defn filter-list
  [values criteria pos]
  (let [fqs (->>
             (map #(nth % pos) values)
             (frequencies))
        bit (let [zeros (get fqs "0" 0)
                  ones (get fqs "1" 0)]
              (if (= criteria :most-common)
                (if (>= ones zeros)
                  "1"
                  "0")
                (if (< ones zeros)
                  "1"
                  "0")))]
    (filter #(= bit (nth % pos)) values)))

(defn find-value
  [values criteria]
  {:pre [(seq? values)
         (vector? (first values))
         (string? (first (first values)))
         (contains? #{:most-common :least-common} criteria)]
   :post [(vector? %)]}
  (first
   (loop [values values
          criteria criteria
          pos 0]
     (if (= 1 (count values))
       values
       (recur (filter-list values criteria pos)
              criteria
              (inc pos))))))

(defn b
  []
  (let [values (map #(str/split % #"") 
                    (str/split-lines (slurp (io/resource "03.txt"))))]
    (->>
     [(find-value values :most-common)
      (find-value values :least-common)]
     (map str/join)
     (flatten)
     (map #(Long/parseLong % 2))
     (apply *))))

(keyword 'most-common)
(keyword 'least-common)
