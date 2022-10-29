(ns aoc-2011.day-17
  (:require [clojure.java.io :refer [resource]]))

(defn parse-input [input]
  (let [[x1 x2 y1 y2] (re-seq #"-?\d+" input)]
    {:x-min (parse-long x1)
     :x-max (parse-long x2)
     :y-min (parse-long y1)
     :y-max (parse-long y2)}))

(defn a
  ([] (a (slurp (resource "17.txt"))))
  ([input]
   (let [{y-min :y-min} (parse-input input)
         speed (dec (abs y-min))]
     (/ (* speed (inc speed)) 2))))

(defn velocities [velocity-fn initial-velocity]
  (let [step-fn (fn [previous-velocity _] (velocity-fn previous-velocity))]
    (reductions step-fn initial-velocity (range))))

(defn next-x-velocity [v] (max (dec v) 0))

(defn next-y-velocity [v] (dec v))

(defn distances [axis initial-velocity max-dist]
  (let [take-pred (if (= axis :x) #(<= % max-dist) #(<= max-dist %))
        next-velocity-fn (if (= axis :x) next-x-velocity next-y-velocity)
        vs (velocities next-velocity-fn initial-velocity)]
    (take-while take-pred (rest (reductions #(+ %1 %2) 0 vs)))))

(defn y-distances [initial-velocity min-y]
  (distances :y initial-velocity min-y))

(defn x-distances [initial-velocity max-x]
  (distances :x initial-velocity max-x))

(defn compute-target-steps-y [velocity y-min y-max]
  (let [distances (y-distances velocity y-min)
        steps (map inc (range))
        dists-steps (map vector distances steps)]
    (->> dists-steps
         (filter #(<= y-min (first %) y-max))
         (map second))))

(defn explode [[velocity steps]]
  (map #(vector velocity %) steps))

(defn comp-velocities-steps-y [y-min y-max]
  (let [y-velocities (range y-min  (abs y-min))]
    (->> (map #(compute-target-steps-y % y-min y-max) y-velocities)
         (map vector y-velocities)
         (mapcat explode)
         (map (fn [t] {:velocity (first t) :steps (second t)})))))

(defn max-steps-from-y-velocities [velocities-steps]
  (->> velocities-steps
       (map :steps)
       (apply max)))

(defn compute-target-steps-x [velocity x-min x-max max-steps-x]
  (let [distances (x-distances velocity x-max)
        steps (take-while #(<= % max-steps-x) (map inc (range)))
        dists-steps (map vector distances steps)]
    (->> dists-steps
         (filter #(<= x-min (first %) x-max))
         (map second))))

(defn comp-velocities-steps-x [x-min x-max max-steps-x]
  (let [min-velocity (long (Math/sqrt (* 2 x-min)))
        max-velocity x-max
        x-velocities (range min-velocity (inc max-velocity))]
    (->> (map #(compute-target-steps-x % x-min x-max max-steps-x) x-velocities)
         (map vector x-velocities)
         (mapcat explode)
         (map (fn [t] {:velocity (first t) :steps (second t)})))))

(defn group-by-steps [v-s-map]
  (-> (group-by :steps v-s-map)
      (update-vals #(map :velocity %))))

(defn initial-velocities-for-step [steps-to-velocities-x steps-to-velocities-y s]
  (let [x-steps (get steps-to-velocities-x s)
        y-steps (get steps-to-velocities-y s)]
    (for [x x-steps y y-steps] (vector x y))))

(defn join-velocities [velocities-steps-x velocities-steps-y]
  (let [steps-to-velocities-x (group-by-steps velocities-steps-x)
        steps-to-velocities-y (group-by-steps velocities-steps-y)
        min-steps (apply min (keys steps-to-velocities-x))
        max-steps (apply max (keys steps-to-velocities-x))
        steps (range min-steps (inc max-steps))]
    (mapcat (partial initial-velocities-for-step steps-to-velocities-x steps-to-velocities-y) steps)))

(defn b
  ([] (b (slurp (resource "17.txt"))))
  ([input]
   (let [{x-min :x-min x-max :x-max
          y-min :y-min y-max :y-max} (parse-input input)
         velocities-steps-y (comp-velocities-steps-y y-min y-max)
         max-steps-x (max-steps-from-y-velocities velocities-steps-y)
         velocities-steps-x (comp-velocities-steps-x x-min x-max max-steps-x)
         velocity-tuples (join-velocities velocities-steps-x velocities-steps-y)]
     (count (distinct velocity-tuples)))))
