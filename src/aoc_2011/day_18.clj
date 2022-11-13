(ns aoc-2011.day-18
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split-lines]]
            [clojure.walk :as walk]
            [clojure.zip :as z]))

(defn parse-input [input]
  (->> (split-lines input)
       (map read-string)))

(defn regular-number? [loc] (number? (z/node loc)))

(defn regular-pair? [loc]
  (let [node (z/node loc)]
    (and (vector? node)
         (number? (first node))
         (number? (second node)))))

(defn find-next-node [loc direction pred]
  (let [next-fn (if (= direction :next) z/next z/prev)
        end? (if (= direction :next) z/end? #(nil? (z/prev %)))]
    (loop [loc (next-fn loc)]
      (if (nil? loc)
        nil
        (if (pred loc)
          loc
          (if (end? loc)
            nil
            (recur (next-fn loc))))))))

(defn explodable-pair? [loc]
  (and (regular-pair? loc)
       (<= 4 (count (z/path loc)))))

(defn next-explodable [loc]
  (find-next-node loc :next explodable-pair?))

(defn explodable? [loc]
  (some? (next-explodable loc)))

(defn next-regular-number [loc]
  (find-next-node loc :next regular-number?))

(defn prev-regular-number [loc]
  (find-next-node loc :prev regular-number?))

(defn explode [loc]
  (-> (if-let [node (next-explodable loc)]
        (let [[l r] (z/node node)
              loc (z/replace node 0)
              loc (if-let [loc (prev-regular-number loc)]
                    (-> (z/edit loc + l)
                        (next-regular-number))
                    loc)
              loc (if-let [loc (next-regular-number loc)]
                    (z/edit loc + r)
                    loc)]
          loc)
        loc)
      (z/root)
      (z/vector-zip)))

(defn splitable-loc? [loc]
  (let [node (z/node loc)]
    (and (number? node)
         (<= 10 node))))

(defn next-splitable [loc]
  (find-next-node loc :next splitable-loc?))

(defn splitable? [loc]
  (some? (next-splitable (z/vector-zip (z/root loc)))))

(defn split [loc]
  (let [loc (next-splitable loc)]
    (-> (z/edit loc #(vector (int (Math/floor (/ % 2)))
                             (int (Math/ceil (/ % 2)))))
        (z/root)
        (z/vector-zip))))

(defn reduce-snf [v]
  (loop [loc (z/vector-zip v)]
    (cond
      (explodable? loc) (recur (explode loc))
      (splitable? loc) (recur (split loc))
      :else (z/root loc))))

(defn add-up [z1 z2]
  (reduce-snf [z1 z2]))

(defn magnitude-fn [x]
  (cond
    (vector? x) (+ (* 3 (first x))
                   (* 2 (second x)))
    (number? x) x))

(defn magnitude [x]
  (walk/postwalk magnitude-fn x))

(defn a
  ([] (a (slurp (resource "18.txt"))))
  ([input]
   (->> (parse-input input)
        (reduce add-up)
        (magnitude))))

(defn b
  ([] (b (slurp (resource "18.txt"))))
  ([input]
   (let [numbers (parse-input input)
         pairs (for [x numbers
                     y numbers
                     :when (not= x y)]
                 (vector x y))]
     (->> pairs
          (map #(apply add-up %))
          (map magnitude)
          (apply max)))))
