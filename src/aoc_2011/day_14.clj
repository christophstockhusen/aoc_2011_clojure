(ns aoc-2011.day-14
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split split-lines]]))

(defn- parse-template [s]
  (let [splitted (split s #"")
        pairs (partition 2 1 splitted)]
    (zipmap pairs (repeat (count pairs) 1))))

(defn- parse-insertion [s]
  (let [splitted (split s #" -> ")
        first-splitted (split (first splitted) #"")]
    [first-splitted (second splitted)]))

(defn- parse-insertions [input]
  (->> input
       (split-lines)
       (map parse-insertion)
       (into (hash-map))))

(defn- parse-input [input]
  (let [splitted (split input #"\n\n")
        template (parse-template (first splitted))
        first-letter (str (first (first splitted)))
        last-letter (str (last (first splitted)))
        insertions (parse-insertions (second splitted))]
    {:template template
     :insertions insertions 
     :first-letter first-letter
     :last-letter last-letter}))

(defn- expand [[[a1 a2] cnt] insertions]
  (let [c (get insertions [a1 a2])]
    [[[a1 c] cnt] [[c a2] cnt]]))

(defn- step [{template :template insertions :insertions} _]
  (let [expanded (mapcat #(expand % insertions) template)
        grouped (group-by first expanded)
        summed (update-vals grouped #(apply + (map second %)))]
    {:template summed :insertions insertions}))

(defn- compute-counts [first-char last-char template]
  (let [exploded (mapcat (fn [[[c1 c2] cnt]] [[c1 cnt] [c2 cnt]]) template)
        grouped (group-by first exploded)
        summed (update-vals grouped #(apply + (map second %)))
        inc-first (update summed first-char inc)
        inc-last (update inc-first last-char inc)
        halfed (update-vals inc-last #(/ % 2))]
    halfed))

(defn- max-minus-min [polymer]
  (let [max-val (apply max (vals polymer))
        min-val (apply min (vals polymer))]
    (- max-val min-val)))

(defn compute-solution [input steps]
  (let [conf (parse-input input)]
    (->> (reduce step (parse-input input) (range steps))
         (:template)
         (compute-counts (:first-letter conf) (:last-letter conf))
         (max-minus-min))))

(defn a
  ([] (a (slurp (resource "14.txt"))))
  ([input] (compute-solution input 10)))

(defn b
  ([] (b (slurp (resource "14.txt"))))
  ([input] (compute-solution input 40)))
