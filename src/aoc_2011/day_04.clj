(ns aoc-2011.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-row
  [s]
  (map (fn [x] {:value (Integer/parseInt x) :marked false})
       (str/split (str/replace (str/trim s) #" +" " ") #" ")))

(defn parse-board
  [s]
  (map parse-row (str/split-lines s)))

(defn unmarked-values
  [board]
  (->> (flatten board)
       (filter #(not (:marked %)))
       (map #(:value %))))

(defn bingo-vector?
  [v]
  (every? :marked v))

(defn column
  [board x]
  (map #(nth % x) board))

(defn columns
  [board]
  (map #(column board %)
       (range (count board))))

(defn rows
  [board]
  board)

(defn bingo?
  [board]
  (->> (concat (rows board) (columns board))
       (some bingo-vector?)
       (boolean)))

(defn update-row
  [row value]
  (map #(if (= (:value %) value)
          {:value value :marked true}
          %)
       row))

(defn update-board
  [board value]
  (map #(update-row % value) board))

(defn update-boards
  [boards value]
  (map #(update-board % value) boards))

(defn first-bingo-board-and-value
  [boards values]
  (loop [boards boards
         values values]
    (let [current-value (first values)
          boards (update-boards boards current-value)
          bingo-board (first (filter bingo? boards))]
      (if (some? bingo-board)
        {:value current-value :board bingo-board}
        (recur boards (rest values))))))

(defn compute-score
  [{value :value board :board}]
  (* value (apply + (unmarked-values board))))

(defn a
  ([] (a (slurp (io/resource "04.txt"))))
  ([input]
   (let [inputs (str/split input #"\n\n")
         values (map #(Integer/parseInt %) (str/split (first inputs) #","))
         boards (map parse-board (rest inputs))]
     (compute-score (first-bingo-board-and-value boards values)))))

(defn last-bingo-board-and-value
  [boards values]
  (loop [boards boards
         values values]
    (let [current-value (first values)
          updated-boards (filter #(not (bingo? %)) (update-boards boards current-value))]
      (if (= 0 (count updated-boards))
        {:value current-value :board (update-board (first boards) current-value)}
        (recur updated-boards (rest values))))))

(defn b
  ([] (b (slurp (io/resource "04.txt"))))
  ([input]
   (let [inputs (str/split input #"\n\n")
         values (map #(Integer/parseInt %) (str/split (first inputs) #","))
         boards (map parse-board (rest inputs))]
     (compute-score (last-bingo-board-and-value boards values)))))
