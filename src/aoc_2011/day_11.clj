(ns aoc-2011.day-11
  (:require [clojure.java.io :as io]
            [clojure.set :refer [difference]]
            [clojure.string :refer [join split split-lines]]))

(defn- parse-line [[row-no line]]
  (->> (split line #"")
       (map parse-long)
       (map (fn [col-no val] (hash-map (vector col-no row-no) val)) (range))
       (apply merge)))

(defn- parse-input [input]
  (->> input
       (split-lines)
       (map (fn [row-no line] (vector row-no line)) (range))
       (map parse-line)
       (apply merge)))

(defn- inc-all [matrix]
  (update-vals matrix inc))

(defn- reset-flashed [matrix]
  (let [update-fn (fn [x] (if (> x 9) 0 x))]
    (update-vals matrix update-fn)))

(defn- flashing [matrix]
  (->> (filter #(> (second %) 9) matrix)
       (map first)))

(defn- size [matrix]
  (int (Math/sqrt (count matrix))))

(defn- neighbors [matrix [x y]]
  (let [deltas [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
    (->> (map #(map + % [x y]) deltas)
         (filter (fn [c] (every? #(< -1 % (size matrix)) c))))))

(defn- inc-neighbors [matrix [x y]]
  (reduce (fn [m c] (update m c inc)) matrix (neighbors matrix [x y])))

(defn matrix-to-string [matrix fmt]
  (->> (range (size matrix))
       (map (fn [y] (map #(vector % y) (range (size matrix)))))
       (map (fn [row] (map (fn [c] (format fmt (get matrix c nil))) row)))
       (map join)
       (join "\n")))

(defn- print-matrix [matrix]
  (print (matrix-to-string matrix "%3d")))

(defn- flashing-neighbors [matrix [x y]]
  (let [n (neighbors matrix [x y])]
    (->> (map #(vector % (get matrix %)) n)
         (filter #(> (second %) 9))
         (map first)
         (map vec))))

(defn- simulate-step [{orig-matrix :matrix flashes :flashes} step]
  (loop [matrix (inc-all orig-matrix)
         q (apply conj (clojure.lang.PersistentQueue/EMPTY) (flashing matrix))
         enqueued (set q)]
    (if (empty? q)
      {:matrix (reset-flashed matrix) :flashes (+ flashes (count enqueued)) :step step}
      (let [current (peek q)
            increased (inc-neighbors matrix current)
            popped-q (pop q)
            newly-flashing (difference (set (flashing-neighbors increased current)) enqueued)
            new-q (apply conj popped-q newly-flashing)
            new-enqueued (apply conj enqueued (set new-q))]
        (recur increased
               new-q
               new-enqueued)))))

(defn- simulate-and-count-flashes [steps matrix]
  (reduce simulate-step {:matrix matrix :flashes 0} (range steps)))

(defn a
  ([] (a (slurp (io/resource "11.txt"))))
  ([input]
   (->> (a input 100)
        (:flashes)))
  ([input steps]
   (->> input
        (parse-input)
        (simulate-and-count-flashes steps))))

(defn- synced? [matrix]
  (every? #(= 0 (second %)) matrix))

(defn b
  ([] (b (slurp (io/resource "11.txt"))))
  ([input]
   (let [parsed (parse-input input)]
     (loop [step 0
            conf {:matrix parsed :flashes 0}]
       (if (synced? (:matrix conf))
         step
         (recur (inc step)
                (simulate-step conf 0)))))))
