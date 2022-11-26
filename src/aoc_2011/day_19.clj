(ns aoc-2011.day-19
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split-lines split]]
            [clojure.set :refer [intersection]]))

(defn parse-scanner [input]
  (let [lines (split-lines input)
        no (re-find #"\d+" (first lines))
        beacons (->> (drop 1 lines)
                     (map #(split % #","))
                     (map #(map parse-long %)))]
    {:no no :beacons beacons}))

(defn parse-input [input]
  (->> (split input #"\n\n")
       (map parse-scanner)))

(defn vec-add [v0 v1]
  (map + v0 v1))

(defn vec-diff [v0 v1]
  (map - v0 v1))

(defn sum-of-squares [v]
  (->> (map #(Math/pow % 2) v)
       (map long)
       (reduce +)))

(defn fingerprint [scanner]
  (let [fingerprints 
        (for [v (:beacons scanner)
              w (:beacons scanner)]
          (sum-of-squares (vec-diff v w)))]
    (assoc scanner :fingerprints fingerprints)))

(defn overlapping? [s0 s1]
  (<= 66 (count (intersection (set (:fingerprints s0)) (set (:fingerprints s1))))))

(defn find-overlapping-scanner [aligned remaining]
  (->> remaining
       (mapcat (fn [r] (mapcat #(if (overlapping? % r)
                                  (list % r)
                                  nil) aligned)))))

(defn rotate [n [x y z]]
  (case n  
    0 [x y z]
    1 [y (- x) z]
    2 [(- x) (- y) z]
    3 [(- y) x z]
    4 [z y (- x)]
    5 [y (- z) (- x)]
    6 [(- z) (- y) (- x)]
    7 [(- y) z (- x)]
    8 [z (- x) (- y)]
    9 [(- x) (- z) (- y)]
    10 [(- z) x (- y)]
    11 [x z (- y)]
    12 [z (- y) x]
    13 [(- y) (- z) x]
    14 [(- z) y x]
    15 [y z x]
    16 [z x y]
    17 [x (- z) y]
    18 [(- z) (- x) y]
    19 [(- x) z y]
    20 [(- x) y (- z)]
    21 [y x (- z)]
    22 [x (- y) (- z)]
    23 [(- y) (- x) (- z)]))

(defn beacon-diffs [beacons]
  (for [v beacons
        w beacons
        :when (not= v w)
        :let [sorted (sort [(vec v) (vec w)])
              diff (apply vec-diff sorted)]]
    {:diff diff :vectors sorted}))

(defn align-scanner-rotation [current-scanner next-scanner]
  (let [curr-beacon-diffs (set (map :diff (beacon-diffs (:beacons current-scanner))))
        nxt-beacons (:beacons next-scanner)
        rotations (map #(partial rotate %) (range 24))
        rotated-beacons (map (fn [rot] (map rot nxt-beacons)) rotations)
        rotated-diffs-with-vs (map beacon-diffs rotated-beacons)
        rotated-diffs (map #(map :diff %) rotated-diffs-with-vs)
        intersections (map #(intersection (set %) curr-beacon-diffs) rotated-diffs)
        counts (map count intersections)
        indexed-counts (map vector (range) counts)
        best-idx (first (apply max-key second indexed-counts))
        rotated-scanner (assoc next-scanner
                               :beacons (nth rotated-beacons best-idx)
                               :beacon-diffs (nth rotated-diffs-with-vs best-idx))]
    rotated-scanner))


(defn set-absolute-beacon-positions [scanner]
  (let [center (:center scanner)
        beacons (:beacons scanner)
        absolute-positions (map #(vec-add center %) beacons)]
    (assoc scanner :absolute-beacon-positions absolute-positions)))

(defn align-scanner-translation [current-scanner next-scanner]
  (let [current-diffs (into (hash-map) (map #(vector (:diff %) (:vectors %)) (:beacon-diffs current-scanner)))
        next-diffs (into (hash-map) (map #(vector (:diff %) (:vectors %)) (:beacon-diffs next-scanner)))
        common-diffs (intersection (set (keys current-diffs)) (set (keys next-diffs)))
        d (first common-diffs)
        current-vs (get current-diffs d)
        next-vs (get next-diffs d)
        c-v0 (first current-vs)
        n-v0 (first next-vs)
        c-center (:center current-scanner)
        n-center (vec-diff (vec-add c-center c-v0) n-v0)]
    (-> (assoc next-scanner :center n-center)
        (set-absolute-beacon-positions))))

(defn align-rotation-and-translation [current-scanner next-scanner]
  (align-scanner-translation current-scanner
                             (align-scanner-rotation current-scanner next-scanner)))

(defn align [scanners]
  (let [s0 (first scanners)
        s0 (assoc s0 
                  :center [0 0 0]
                  :beacon-diffs (beacon-diffs (:beacons s0))
                  :absolute-beacon-positions (:beacons s0))]
    (loop [aligned #{s0}
           remaining (set (drop 1 scanners))]
      (if (empty? remaining)
        aligned
        (let [[current-scanner next-scanner] (find-overlapping-scanner aligned remaining)
              aligned-scanner (align-rotation-and-translation current-scanner next-scanner)]
          (recur (conj aligned aligned-scanner)
                 (disj remaining next-scanner)))))))

(defn a
  ([] (a (slurp (resource "19.txt"))))
  ([input]
   (->> (parse-input input)
        (map fingerprint)
        (align)
        (mapcat :absolute-beacon-positions)
        (into #{})
        (count))))

(defn manhatten-dist [v w]
  (reduce + (map abs (vec-diff v w))))

(defn b
  ([] (b (slurp (resource "19.txt"))))
  ([input]
   (let [centers (->> (parse-input input)
                      (map fingerprint)
                      (align)
                      (map :center))]
     (apply max (for [c0 centers
                      c1 centers
                      :when (not= c0 c1)]
            (manhatten-dist c0 c1))))))
