(ns aoc-2011.day-18
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split-lines]]
            [clojure.zip :as z]))

;; (remove-ns 'aoc-2011.day-18)

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

(defn next-regular-pair [loc] (find-next-node loc :next regular-pair?))

(let [loc (z/vector-zip [0 [1 2] 10])]
  (find-next-node loc :next regular-pair?))

(let [loc (z/vector-zip [0 [1 2] 10])]
  (-> loc
      ;; (z/next)
      ;; (z/next)
      ;; (z/next)
      ;; (z/next)
      ;; (z/next)
      ;; (z/next)
      (find-next-node :next splitable-loc?)))

(defn explodable-pair? [loc]
  (and (regular-pair? loc)
       (< 4 (count (z/path loc)))))

(defn next-explodable [loc]
  (find-next-node loc :next explodable-pair?))

(defn explodable? [loc]
  (some? (next-explodable loc)))

(defn next-regular-number [loc]
  (find-next-node loc :next regular-number?))

(defn prev-regular-number [loc]
  (find-next-node loc :prev regular-number?))


(defn explode [loc]
  (if-let [node (next-explodable loc)]
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
    loc))

(defn splitable-loc? [loc]
  (let [node (z/node loc)]
    (and (number? node)
         (<= 10 node))))

(defn next-splitable [loc]
  (find-next-node loc :next splitable-loc?))

(defn splitable? [loc]
  (some? (next-splitable (z/root loc))))

(defn split [loc]
  (z/edit loc #(vector (int (Math/ceil (/ % 2)))
                       (int (Math/floor (/ % 2))))))

(defn add-up [z1 z2]
  (let [loc (z/vector-zip [z1 z2])]
    (loop [loc loc]
      (cond
        (explodable? loc) (recur (explode loc))
        (splitable? loc) (recur (split loc))
        :else (z/node (z/root loc))))))

(def z1 "[2 [[7 4] [5 [3 9]]]]")
(def z2 "[[[[1 4] [0 1]] 4] [3 [8 5]]]")

(add-up z1 z2)

(defn a
  ([] (a (slurp (resource "18.txt"))))
  ([input]
   (->> (parse-input input)
        ;; (reduce add-up)
        ;; (magnitude)
        )))

(take 10 (a))