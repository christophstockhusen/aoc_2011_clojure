(ns aoc-2011.day-25 
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-point [x y c]
  [[x y] (if (= c \>) :east :south)])

(defn parse-line [row-no line]
  (let [points (->> (seq line)
                    (map vector (range))
                    (filter #(not= \. (second %)))
                    (map #(parse-point (first %) row-no (second %))))
        east-facing (into {} (filter #(= :east (second %)) points))
        south-facing (into {} (filter #(= :south (second %)) points))]
    {:east-facing east-facing :south-facing south-facing}))

(defn parse-input [input]
  (let [splitted (str/split-lines input)
        max-y (dec (count splitted))
        max-x (dec (count (first splitted)))
        cucumbers (->> splitted
                       (map parse-line (range))
                       (apply merge-with merge))]
    {:max-x max-x 
     :max-y max-y 
     :east-facing (:east-facing cucumbers) 
     :south-facing (:south-facing cucumbers)}))

(defn right [max-x x] (mod (inc x) (inc max-x)))

(defn down [max-y y] (mod (inc y) (inc max-y)))

(defn next-coordinate [max-x max-y [[x y] direction]]
  (if (= :east direction)
    [(right max-x x) y]
    [x (down max-y y)]))

(defn step [{max-x :max-x 
             max-y :max-y 
             east-facing :east-facing 
             south-facing :south-facing}
            [[old-x old-y] direction]]
  (let [[x y] (next-coordinate max-x max-y [[old-x old-y] direction])
        free? (and (not (contains? east-facing [x y]))
                   (not (contains? south-facing [x y])))]
    [free? [(if free? [x y] [old-x old-y]) direction]]))

(defn simulate-step [{max-x :max-x
                      max-y :max-y
                      east-facing :east-facing
                      south-facing :south-facing
                      :as conf}]
  (let [stepped-east (map (partial step conf) east-facing)
        east-facing (into {} (map second stepped-east))
        conf' (assoc conf :east-facing east-facing)
        stepped-south (map (partial step conf') south-facing)
        south-facing (into {} (map second stepped-south))
        moved (reduce #(or %1 %2) (map first (concat stepped-east stepped-south)))]
    [moved {:max-x max-x 
            :max-y max-y 
            :east-facing east-facing 
            :south-facing south-facing}]))


(defn print-sea [{max-x :max-x max-y :max-y east-facing :east-facing south-facing :south-facing}]
  (let [cucumbers (merge east-facing south-facing)]
    (->> (range (inc max-y))
         (map (fn [row-no] (map vector (range (inc max-x)) (repeat row-no))))
         (map (fn [row] (map #(case (get cucumbers %)
                                :east ">"
                                :south "v"
                                ".") row)))
         (map str/join)
         (str/join "\n")
         (println))))

(defn a
  ([] (a (slurp (io/resource "25.txt"))))
  ([input]
   (let [conf (parse-input input)]
     (loop [step 1
            conf conf]
       (let [[moved nxt] (simulate-step conf)]
         (if moved
           (recur (inc step) nxt)
           (do (print-sea nxt)
               step)))))))
