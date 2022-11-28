(ns aoc-2011.day-21 
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [input]
  (let [[p1 p2] (str/split-lines input)]
    {:p1 (parse-long (second (re-seq #"\d+" p1)))
     :p2 (parse-long (second (re-seq #"\d+" p2)))}))

(defn faces [player]
  (let [drops (case player
                :p1 1
                :p2 4)]
    (->> (range)
         (drop drops)
         (partition 3 6)
         (map #(apply + %)))))

(defn step [{pos :pos score :score} face-sum]
  (let [pos (mod (+ pos face-sum) 10)
        next-score (if (= pos 0) 10 pos)]
    {:pos pos :score (+ score next-score)}))

(defn confs [player pos]
  (reductions step {:pos pos :score 0} (faces player)))

(defn first-winning-conf [steps]
  (first (keep-indexed (fn [idx conf] (if (<= 1000 (:score conf)) {:step idx :conf conf})) 
                       steps)))

(defn first-to-1000 [{p1 :p1 p2 :p2}]
  (let [p1-steps (confs :p1 p1)
        p2-steps (confs :p2 p2)
        {p1-idx :step} (first-winning-conf p1-steps)
        {p2-idx :step} (first-winning-conf p2-steps)]
    (if (< p1-idx p2-idx)
      (* (- (* 6 p1-idx) 3) (:score (nth p2-steps (dec p1-idx))))
      (* 6 p2-idx (:score (nth p1-steps (dec p2-idx)))))))

(defn a
  ([] (a (slurp (io/resource "21.txt"))))
  ([input] (->> (parse-input input)
                (first-to-1000))))

(def quantum 
  (memoize
   (fn [pos score other-pos other-score]
     (let [outcomes (for [x (range 1 4)
                          y (range 1 4)
                          z (range 1 4)]
                      (+ x y z))
           positions (map #(mod (+ pos %) 10) outcomes)
           scores (map #(+ score (if (= 0 %) 10 %)) positions)]
       (reverse (reduce #(map + %1 %2)
                        (map (fn [p s] (if (<= 21 s) 
                                         [0 1] 
                                         (quantum other-pos other-score p s)))
                             positions
                             scores)))))))

(defn b
  ([] (b (slurp (io/resource "21.txt"))))
  ([input] (let [{p1 :p1 p2 :p2} (parse-input input)]
             (apply max (quantum p1 0 p2 0)))))
