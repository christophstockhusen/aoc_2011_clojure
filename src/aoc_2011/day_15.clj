(ns aoc-2011.day-15
  (:require [clojure.java.io :refer [resource]]
            [clojure.set :refer [difference]]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :refer [join split split-lines]]))

(defn- parse-line [line row-no]
  (let [cols (split line #"")]
    (->> (map (fn [x v] [[x row-no] (parse-long v)]) (range (count cols)) cols)
         (into (hash-map)))))

(defn- dimensions [matrix]
  [(inc (apply max (map first (keys matrix))))
   (inc (apply max (map second (keys matrix))))])

(defn parse-input [input]
  (let [lines (split-lines input)
        matrix (->> (map parse-line lines (range (count lines)))
                    (apply merge))
        [max-x max-y] (dimensions matrix)]
    {:max-x max-x :max-y max-y :matrix matrix}))

(defn neighbors [max-x max-y v]
  (let [deltas [[-1 0] [0 1] [1 0] [0 -1]]]
    (filter #(and (< -1 (first %) max-x)
                  (< -1 (second %) max-y))
            (map #(map + v %) deltas))))

(defn min-direct-dist [q matrix d t]
  (let [enqueued-dist (get q t Long/MAX_VALUE)
        s-t-dist (+ d (get matrix t))]
    (min enqueued-dist s-t-dist)))

;; (b input)

(defn neighbors-with-dists [{max-x :max-x max-y :max-y matrix :matrix} visited q current-v current-d]
  (let [v-neighs (set (neighbors max-x max-y current-v))
        unvisited-neighs (difference v-neighs visited)
        unvisited-neighs-dists (map #(min-direct-dist q matrix current-d %) unvisited-neighs)]
    (map vector unvisited-neighs unvisited-neighs-dists)))

(defn compute-risks [conf]
  (loop [dists {}
         visited #{}
         q (priority-map [0 0] 0)]
    (if (empty? q)
      dists
      (let [[current-v current-d] (peek q)
            new-dists (assoc dists current-v current-d)
            new-visited (conj visited current-v)
            new-neighbor-dists (neighbors-with-dists conf new-visited q current-v current-d)
            new-q (into (pop q) new-neighbor-dists)]
        (recur new-dists new-visited new-q)))))

(defn compute-lowest-total-risk [{max-x :max-x max-y :max-y :as conf}]
  (let [risks (compute-risks conf)]
    (get risks [(dec max-x) (dec max-y)])))

(defn matrix-to-string [matrix fmt]
  (->> (range (second (dimensions matrix)))
       (map (fn [y] (map #(vector % y) (range (first (dimensions matrix))))))
       (map (fn [row] (map (fn [c] (format fmt (get matrix c -1))) row)))
       (map join)
       (join "\n")))

(defn- print-matrix [matrix]
  (println (matrix-to-string matrix "%3d")))

(defn a
  ([] (a (slurp (resource "15.txt"))))
  ([input]
   (->> (parse-input input)
        (compute-lowest-total-risk))))

(defn expansion-indices []
  (->> (range 5)
       (mapcat (fn [row-no] (map #(vector % row-no) (range 5))))))

(defn increase-risk-value [v x]
  (let [increased (+ v x)
        overflow (< 9 increased)
        v' (mod increased 10)]
    (if overflow (inc v') increased)))

(defn expand-matrix-for-index [{max-x :max-x max-y :max-y matrix :matrix} [idx-x idx-y]]
  (map (fn [[[m-x m-y] val]]
         [[(+ m-x (* idx-x max-x)) (+ m-y (* idx-y max-y))]
          (increase-risk-value val (+ idx-x idx-y))])
       matrix))

(defn expand-matrix [{max-x :max-x max-y :max-y matrix :matrix :as conf}]
  (let [expansion-idcs (expansion-indices)
        expanded-matrix (->> (mapcat #(expand-matrix-for-index conf %) expansion-idcs)
                             (into (hash-map)))]
    {:max-x (* 5 max-x) :max-y (* 5 max-y) :matrix expanded-matrix}))

(defn b
  ([] (b (slurp (resource "15.txt"))))
  ([input]
   (let [expanded-matrix (->> (parse-input input)
                              (expand-matrix))]
     (compute-lowest-total-risk expanded-matrix))))
