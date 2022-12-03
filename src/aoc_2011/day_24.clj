(ns aoc-2011.day-24 
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-block [block]
  (let [parse-fn (fn [l] (parse-long (re-find #"-?\d+" (nth block l))))
        a (parse-fn 4)
        b (parse-fn 5)
        c (parse-fn 15)]
    {:a a :b b :c c}))

(defn parse-input [input]
  (->> (str/split-lines input)
       (partition 18)
       (map parse-block)))

(defn block [w z a b c]
  (if (not= (+ b (mod z 26)) w)
    (+ (* 26 (quot z a)) w c)
    (quot z a)))

(def search-fn
  (memoize
   (fn [mode params i z]
     (if (not (contains? #{:smallest :largest} mode))
       (throw (IllegalArgumentException. "mode must be :smallest or :largest"))
       (let [remaining-divs (count (filter #(= 26 %) (map :a (drop i params))))
             budget (long (Math/pow 26 remaining-divs))]
         (if (<= budget z)
           nil
           (if (= i 14)
             (if (= z 0)
               []
               nil)
             (let [{a :a b :b c :c} (nth params i)
                   ws (cond->> (range 1 10)
                        (= mode :largest) (reverse)
                        (= mode :smallest) (identity))
                   zs (map #(block % z a b c) ws)
                   ts (map vector ws zs)]
               (first (keep #(if-let [res (search-fn mode params (inc i) (second %))]
                               (conj res (first %))) ts))))))))))

(defn search [mode params]
  (search-fn mode params 0 0))

(defn a 
  ([] (a (slurp (io/resource "24.txt"))))
  ([input] (str/join (reverse (search :largest (parse-input input))))))

(defn b
  ([] (b (slurp (io/resource "24.txt"))))
  ([input] (str/join (reverse (search :smallest (parse-input input))))))