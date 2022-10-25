(ns aoc-2011.day-16
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [join split]]))

(def hex-map
  {"0" "0000"
   "1" "0001"
   "2" "0010"
   "3" "0011"
   "4" "0100"
   "5" "0101"
   "6" "0110"
   "7" "0111"
   "8" "1000"
   "9" "1001"
   "A" "1010"
   "B" "1011"
   "C" "1100"
   "D" "1101"
   "E" "1110"
   "F" "1111"})

(defn hex-to-binary [s]
  (mapcat #(split (get hex-map (str %)) #"") s))

(defn bits->int [bits]
  (Long/parseLong (join bits) 2))

(defn parse-version [bits]
  (let [version-bits (take 3 bits)
        version-id (bits->int version-bits)]
    {:version-id version-id :next-bits (drop 3 bits)}))

(defn parse-type [bits]
  (let [type-bits (take 3 bits)
        type-id (bits->int type-bits)]
    {:type-id type-id :next-bits (drop 3 bits)}))

(defn parse-payload-literal [bits]
  (loop [bits bits
         bits-read []
         num-bits-read 0]
    (let [leading (first bits)
          group (take 5 bits)
          bits-read (concat bits-read (drop 1 group))
          remaining (drop 5 bits)
          num-bits-read (+ 5 num-bits-read)]
      (if (= "0" leading)
        {:literal (bits->int bits-read) :next-bits remaining :packet-length num-bits-read}
        (recur remaining
               bits-read
               num-bits-read)))))

(defn parse-subpacket-length [bits]
  (let [length-type-id (first bits)
        remaining (drop 1 bits)]
    (if (= length-type-id "0")
      {:sub-packets-length (bits->int (take 15 remaining))
       :next-bits (drop 15 remaining)
       :num-bits-read-payload-size 16}
      {:sub-packets-count (bits->int (take 11 remaining))
       :next-bits (drop 11 remaining)
       :num-bits-read-payload-size 12})))

(defn literal-id? [x]
  (= 4 x))

(defn literal-packet? [p]
  (literal-id? (:type-id p)))

(let [{foo :foo} {}]
  (nil? foo))

(defn append-to-top-stack-body [stack pkg]
  (let [top (peek stack)
        updated (update top :sub-packets #(conj % pkg))]
    (conj (pop stack) updated)))

(defn fully-parsed? [packet]
  (let [sub-packets (:sub-packets packet)
        expected-sub-packets-length (:sub-packets-length packet)
        expected-sub-packets-count (:sub-packets-count packet)
        actual-sub-packets-length (apply + (map #(:packet-length %) sub-packets))
        actual-sub-packets-count (count sub-packets)]
    (or (= expected-sub-packets-count actual-sub-packets-count)
        (= expected-sub-packets-length actual-sub-packets-length))))

(defn set-subpackets-length [packet]
  (if (nil? (:sub-packets-length packet))
    (assoc packet :sub-packets-length (apply + (map #(:packet-length %) (:sub-packets packet))))
    packet))

(defn set-subpackets-count [packet]
  (if (nil? (:sub-packets-count packet))
    (assoc packet :sub-packets-count (count (:sub-packets packet)))
    packet))

(defn set-operator-packet-size [packet]
  (assoc packet :packet-length (+ (:sub-packets-length packet)
                                  (:num-bits-read-payload-size packet)
                                  6)))

(defn complete-operator-packet-info [packet]
  (->> packet
       (set-subpackets-count)
       (set-subpackets-length)
       (set-operator-packet-size)))

(defn finish-packet [packet]
  (if (literal-packet? packet)
    packet
    (complete-operator-packet-info packet)))

(defn parse [bits]
  (loop [stack []
         next-bits bits]
    (if (or (literal-packet? (peek stack))
            (fully-parsed? (peek stack)))
      (let [current (peek stack)
            new-stack (pop stack)
            finished-packet (finish-packet current)]
        (if (empty? new-stack)
          finished-packet
          (recur (append-to-top-stack-body new-stack finished-packet)
                 next-bits)))
      (let [{version-id :version-id next-bits :next-bits} (parse-version next-bits)
            {type-id :type-id next-bits :next-bits} (parse-type next-bits)]
        (if (literal-id? type-id)
          (let [body (parse-payload-literal next-bits)
                body-with-header (assoc body :version-id version-id :type-id type-id)
                current (update body-with-header :packet-length #(+ % 6))]
            (if (nil? (peek stack))
              current
              (recur (append-to-top-stack-body stack current)
                     (:next-bits current))))
          (let [payload-size (parse-subpacket-length next-bits)
                current (assoc payload-size :version-id version-id :type-id type-id :sub-packets [])]
            (recur (conj stack current)
                   (:next-bits current))))))))

(defn all-children-evaluated? [{packet :packet values :values}]
  (= (count (:sub-packets packet)) (count values)))

(defn version-sum-fn [node]
  (let [version-id (:version-id (:packet node))]
    (if (literal-packet? (:packet node))
      version-id
      (+ version-id (apply + (:values node))))))

(defn compute-version-sum [compute-fn packet]
  (loop [stack [{:packet packet}]]
    (let [current (peek stack)]
      (if (or (literal-packet? (:packet current))
              (all-children-evaluated? current))
        (let [value (compute-fn current)
              stack (pop stack)]
          (if (empty? stack)
            value
            (let [parent (peek stack)
                  updated-parent (update parent :values #(conj (vec %) value))
                  stack (conj (pop stack) updated-parent)]
              (recur stack))))
        (let [current-packet (:packet current)
              next-child (nth (:sub-packets current-packet) (count (:values current)))
              next-node {:packet next-child :values []}]
          (recur (conj stack next-node)))))))

(defn a
  ([] (a (slurp (resource "16.txt"))))
  ([input]
   (->> (hex-to-binary input)
        (parse)
        (compute-version-sum version-sum-fn))))

(defn eval-fn [node]
  (let [packet (:packet node)]
    (if (literal-packet? packet)
      (:literal packet)
      (let [type-id (:type-id packet)
            values (:values node)]
        (cond
          (= type-id 0) (apply + values)
          (= type-id 1) (apply * values)
          (= type-id 2) (apply min values)
          (= type-id 3) (apply max values)
          (= type-id 5) (if (apply > values) 1 0)
          (= type-id 6) (if (apply < values) 1 0)
          (= type-id 7) (if (apply = values) 1 0))))))

(defn b
  ([] (b (slurp (resource "16.txt"))))
  ([input]
   (->> (hex-to-binary input)
        (parse)
        (compute-version-sum eval-fn))))
