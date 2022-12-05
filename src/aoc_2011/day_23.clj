(ns aoc-2011.day-23 
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-input [input]
  (let [amphipods (map keyword (re-seq #"\w" input))
        levels (reverse (partition 4 amphipods))
        rooms (into {} (map vector
                            [:A :B :C :D]
                            (apply map vector levels)))]
    (assoc rooms
           :depth (count levels)
           :hallway {}
           :room-pos {:A 2 :B 4 :C 6 :D 8}
           :costs {:A 1 :B 10 :C 100 :D 1000})))

(defn pure-room? [[room-name content]]
  (every? #(= room-name %) content))

(defn organized-room? [depth [room-name content]]
  (and (pure-room? [room-name content])
       (= (count content) depth)))

(defn organized? [conf]
  (let [room-types [:A :B :C :D]
        rooms (map #(vector % (get conf %)) room-types)]
    (every? (partial organized-room? (:depth conf)) rooms)))

(defn move-home [{hallway :hallway room-pos :room-pos costs :costs depth :depth :as conf} [pos t]]
  (let [target-hallway-pos (get room-pos t)
        hallway-idxs (range (min target-hallway-pos pos)
                            (inc (max target-hallway-pos pos)))
        hallway-without-current-amphipod (dissoc hallway pos)
        hallway-accessible? (reduce #(and %1 %2)
                                    (map #((complement contains?) hallway-without-current-amphipod %)
                                         hallway-idxs))
        target-room [t (get conf t)]
        room-accessible? (pure-room? target-room)
        room-depth (- depth (count (second target-room)))]
    (if (and hallway-accessible? room-accessible?)
      {:conf (assoc conf
                    :hallway hallway-without-current-amphipod
                    t (conj (get conf t) t))
       :costs (* (get costs t) (+ room-depth (dec (count hallway-idxs))))})))

(defn reachable-hallway-pos [{hallway :hallway room-pos :room-pos} room-type]
  (let [pos (get room-pos room-type)
        leftmost-free-hallway-idx (inc (or (first (first (reverse (sort-by first (filter #(< (first %) pos) hallway))))) -1))
        rightmost-free-hallway-idc (dec (or (first (first (sort-by first (filter #(< pos (first %)) hallway)))) 11))
        hallway-pos (set (range leftmost-free-hallway-idx (inc rightmost-free-hallway-idc)))
        room-blocked-pos (set (map second room-pos))]
    (set/difference hallway-pos room-blocked-pos)))

(defn leave-room [{hallway :hallway room-pos :room-pos costs :costs depth :depth :as conf} room-type]
  (if (and (not-empty (get conf room-type))
           (not (pure-room? [room-type (get conf room-type)])))
      (let [pos (get room-pos room-type)
            reachable (reachable-hallway-pos conf room-type)
            room-escape-costs (- (inc depth) (count (get conf room-type)))
            amphipod-type (peek (get conf room-type))
            reach-costs (map #(* (get costs amphipod-type) (+ room-escape-costs (abs (- pos %)))) reachable)
            reachable-with-costs (map vector reachable reach-costs)] 
        (map (fn [[p c]] {:conf (assoc conf
                                       :hallway (assoc hallway p amphipod-type)
                                       room-type (pop (get conf room-type)))
                          :costs c}) reachable-with-costs))))

(defn next-configurations [{hallway :hallway :as conf}]
  (let [home-moves (keep #(move-home conf %) hallway)
        hallway-moves (mapcat #(leave-room conf %) [:A :B :C :D])]
    (concat home-moves hallway-moves)))

(def least-energy
  (memoize
   (fn [conf]
     (if (organized? conf)
       (list {:costs 0 :conf conf})
       (let [next-confs (next-configurations conf)
             m (map (fn [{nxt-conf :conf costs :costs}]
                      (let [confs (least-energy nxt-conf)]
                        (conj confs {:conf conf :costs (+ costs (:costs (first confs)))}))) next-confs)]
         (if (empty? m)
           (list {:conf nil :costs Integer/MAX_VALUE})
           (apply min-key #(:costs (first %)) m)))))))

(defn print-conf [{hallway :hallway :as conf}]
  (println (str/join "\n" [(str/join " " (map #(get hallway % ".") (range 11)))
                           (:A conf)
                           (:B conf)
                           (:C conf)
                           (:D conf)]) "\n"))

(defn main [input]
  (:costs (first (least-energy (parse-input input)))))

(defn a
  ([] (a (slurp (io/resource "23-a.txt"))))
  ([input] (main input)))

(defn b
  ([] (b (slurp (io/resource "23-b.txt"))))
  ([input] (main input)))
