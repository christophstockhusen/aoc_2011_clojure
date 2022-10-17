(ns aoc-2011.day-12
  (:require [clojure.java.io :refer [resource]]
            [clojure.set :refer [difference intersection]]
            [clojure.string :refer [lower-case split split-lines upper-case]]))

(defn parse-edges [input]
  (->> input
       (split-lines)
       (map #(split % #"-"))))

(defn reverse-vectors [vs]
  (map (fn [v] (vector (second v) (first v))) vs))

(defn small-caves-from-edges [edges]
  (->> (flatten edges)
       (filter #(= % (lower-case %)))
       (set)))

(defn large-caves-from-edges [edges]
  (->> (flatten edges)
       (filter #(= % (upper-case %)))
       (set)))

(defn edge-vectors-to-map [vs]
  (let [grouped (group-by first vs)]
    (update-vals grouped (fn [x] (set (map second x))))))

(defn- parse-input [input]
  (let [parsed-edges (parse-edges input)
        reversed-edges (reverse-vectors parsed-edges)
        edges (edge-vectors-to-map (set (concat parsed-edges reversed-edges)))
        small-caves (small-caves-from-edges parsed-edges)
        large-caves (large-caves-from-edges parsed-edges)]
    {:edges edges :small-caves small-caves :large-caves large-caves}))

(defn next-caves-a [{edges :edges small-caves :small-caves} current-path]
  (let [visited-small-caves (intersection small-caves (set current-path))]
    (difference (get edges (peek current-path)) visited-small-caves)))

(defn count-paths
  ([conf next-caves-fn]
   (count-paths conf (vector "start") next-caves-fn))
  ([conf current-path next-caves-fn]
   (if (= "end" (peek current-path))
     1
     (let [next-avail-caves (next-caves-fn conf current-path)]
       (if (= 0 (count next-avail-caves))
         0
         (apply + (map #(count-paths conf (conj current-path %) next-caves-fn) next-avail-caves)))))))

(defn a
  ([] (a (slurp (resource "12.txt"))))
  ([input]
   (count-paths (parse-input input) next-caves-a)))

(defn next-caves-b [{edges :edges small-caves :small-caves} current-path]
  (let [visited-small-caves (intersection small-caves (set current-path))
        visited-freq (frequencies current-path)
        small-caves-visit-counts (set (vals (filter #(contains? small-caves (first %)) visited-freq)))]
    (if (contains? small-caves-visit-counts 2)
      (difference (get edges (peek current-path)) visited-small-caves)
      (difference (get edges (peek current-path)) #{"start"}))))

(defn b
  ([] (b (slurp (resource "12.txt"))))
  ([input]
   (count-paths (parse-input input) next-caves-b)))
