(ns aoc-2011.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split]]))

(def p-score
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(defn- merge-symbol [{stack :stack} c]
  (let [p (peek stack)]
    (cond
      (contains? #{"(" "[" "{" "<"} c) {:stack (conj stack c)}
      (and (= p "(")
           (= c ")")) {:stack (pop stack)}
      (and (= p "[")
           (= c "]")) {:stack (pop stack)}
      (and (= p "{")
           (= c "}")) {:stack (pop stack)}
      (and (= p "<")
           (= c ">")) {:stack (pop stack)}
      :else (reduced {:stack stack :next c}))))

(defn- reduce-line
  [line]
  (->> (split line #"")
       (reduce merge-symbol {:stack () :next nil})))

(defn- compute-error-score
  [line]
  (as-> (reduce-line line) $
    (get p-score (:next $) 0)))

(defn a
  ([] (a (slurp (io/resource "10.txt"))))
  ([input]
   (->> input
        (split-lines)
        (map compute-error-score)
        (apply +))))

(def p-completion-score
  {"(" 1
   "[" 2
   "{" 3
   "<" 4})

(defn- add-symbol-to-score
  [score symbol]
  (+ (* score 5) (get p-completion-score symbol)))

(defn line-completion-score
  [incomplete-line]
  (reduce add-symbol-to-score 0 incomplete-line))

(defn- middle-element
  [s]
  (nth s (/ (count s) 2)))

(defn b
  ([] (b (slurp (io/resource "10.txt"))))
  ([input]
   (->> input
        (split-lines)
        (map reduce-line)
        (filter #(not (some? (:next %))))
        (map :stack)
        (map line-completion-score)
        (sort)
        (middle-element))))
