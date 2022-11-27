(ns aoc-2011.day-20
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn light-indices [line]
  (keep-indexed #(if (= %2 \#) %1) line))

(defn parse-algorithm [input]
  (set (light-indices input)))

(defn parse-image [input]
  (->> (str/split-lines input)
       (map light-indices)
       (mapcat (fn [row-no line] (map #(vector (vector % row-no) true) line)) (range))
       (into (hash-map))))

(defn pixel-group [c]
  (let [deltas (for [x [-1 0 1]
                     y [-1 0 1]]
                 [x y])]
    (map #(map + c %) deltas)))

(defn parse-input [input]
  (let [[algo-str image-str] (str/split input #"\n\n")
        algorithm (parse-algorithm algo-str)
        image (parse-image image-str)]
    {:image image :algorithm algorithm :background false}))

(defn sort-pixels [coll]
  (sort-by #(vector (last %) (first %)) coll))

(defn update-pixel [algorithm image background [x y]]
  (let [pixel (sort-pixels (pixel-group [x y]))
        values (map #(get image % background) pixel)
        idx (Long/parseLong (str/join (map #(if % 1 0) values)) 2)]
    (contains? algorithm idx)))

(defn update-background [algorithm background]
  (if background
    (contains? algorithm 511)
    (contains? algorithm 0)))

(defn enhance [{image :image algorithm :algorithm background :background}]
  (let [relevant-coordinates (sort-pixels (vec (set (mapcat pixel-group (keys image)))))
        pixel-values (map #(update-pixel algorithm image background %) relevant-coordinates)
        new-image (into (hash-map) (map vector relevant-coordinates pixel-values))
        new-background (update-background algorithm background)]
    {:image new-image :algorithm algorithm :background new-background}))

(defn print-image [image]
  (println image)
  (let [k (keys image)
        min-x (apply min (map first k))
        max-x (apply max (map first k))
        min-y (apply min (map second k))
        max-y (apply max (map second k))]
    (->> (range min-y (inc max-y))
         (map (fn [row] (map #(vector % row) (range min-x (inc max-x)))))
         (map (fn [row] (map #(if (get image % false) "#" ".") row)))
         (map #(str/join %))
         (str/join "\n")
         (println))))

(defn a
  ([] (a (slurp (resource "20.txt"))))
  ([input]
   (let [parsed (parse-input input)
         enhanced (nth (iterate enhance parsed) 2)
         image (:image enhanced)]
     (count (filter #(get image %) (keys image))))))

(defn b
  ([] (b (slurp (resource "20.txt"))))
  ([input]
   (let [parsed (parse-input input)
         enhanced (nth (iterate enhance parsed) 50)
         image (:image enhanced)]
     (count (filter #(get image %) (keys image))))))
