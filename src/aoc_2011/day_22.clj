(ns aoc-2011.day-22 
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-line [line]
  (let [[cmd coordinates] (str/split line #" ")
        [xmin xmax ymin ymax zmin zmax] (map parse-long (re-seq #"-?\d+" coordinates))]
    {:cmd cmd 
     :x [xmin xmax] 
     :y [ymin ymax] 
     :z [zmin zmax]}))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn cubes [xmin xmax ymin ymax zmin zmax]
  (->> (for [x (range (max xmin -50) (inc (min xmax 50)))
             y (range (max ymin -50) (inc (min ymax 50)))
             z (range (max zmin -50) (inc (min zmax 50)))]
         [x y z])
       (set)))

(defn add-step [actives {cmd :cmd [xmin xmax] :x [ymin ymax] :y [zmin zmax] :z}]
  (let [cs (cubes xmin xmax ymin ymax zmin zmax)]
    (if (= cmd "on")
      (set/union actives cs)
      (set/difference actives cs))))

(defn reboot [cmds]
  (reduce add-step #{} cmds))

(defn a 
  ([] (a (slurp (io/resource "22.txt"))))
  ([input] (count (reboot (parse-input input)))))

(defn intersection [[a1 a2] [b1 b2]]
  [(max a1 b1) (min a2 b2)])

(defn cancelling-intersection-cuboid [{cmd-x :x cmd-y :y cmd-z :z}
                                      {cub-type :type cub-x :x cub-y :y cub-z :z}]
  (let [intersection {:x (intersection cmd-x cub-x)
                      :y (intersection cmd-y cub-y)
                      :z (intersection cmd-z cub-z)}]
    (if (every? #(<= (first %) (second %)) (vals intersection))
      [(assoc intersection :type (* -1 cub-type))]))
  )

(defn add [cuboids cmd]
  (let [cancelling-cuboids (mapcat #(cancelling-intersection-cuboid cmd %) cuboids)
        new-cuboids (into cuboids cancelling-cuboids)]
    (if (= "on" (:cmd cmd))
      (conj new-cuboids (assoc (dissoc cmd :cmd) :type 1))
      new-cuboids)))

(defn size [{x :x y :y z :z t :type}]
  (* t (reduce * (map #(- (inc (second %)) (first %)) [x y z]))))

(defn count-on-cubes [cmds]
  (->> (reduce add [] cmds)
       (map size)
       (reduce +)))

(defn b
  ([] (b (slurp (io/resource "22.txt"))))
  ([input] (count-on-cubes (parse-input input))))
