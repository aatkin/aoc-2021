(ns aatkin.day-3
  (:require [clojure.string :as s]))

(def ^:private input (slurp "resources/input/day_3"))
(def ^:private mock-input (slurp "resources/mock_input/day_3"))

(defn- parse [input]
  (->> (s/split-lines input)))

(defn- get-nth-bits [data n]
  (map #(nth % n) data))

(defn- find-bit-average [coll]
  (/ (apply + (map #(Character/digit % 10) coll))
     (count coll)))

(defn- most-common-bit [avg]
  (if (<= 0.5 avg) 1 0))

(defn- least-common-bit [avg]
  (if (<= 0.5 avg) 0 1))

(defn- gamma-rate [coll]
  (->> (range (count (first coll)))
       (map #(get-nth-bits coll %))
       (map find-bit-average)
       (map most-common-bit)))

(defn- epsilon-rate [coll]
  (->> (range (count (first coll)))
       (map #(get-nth-bits coll %))
       (map find-bit-average)
       (map least-common-bit)))

(defn- to-integer [coll]
  (Integer/parseInt (s/join coll) 2))

(defn part-1-solution []
  (->> ((juxt gamma-rate epsilon-rate) (parse input))
       (map to-integer)
       (apply *)))

(comment
  (def coll (parse mock-input))
  (gamma-rate coll)

  (->> ((juxt gamma-rate epsilon-rate) (parse mock-input))
       (map to-integer)
       (apply *))
  
  (part-1-solution)
  )

(defn- by-val-and-pos [coll pos n]
  (filter #(= n (Character/digit (nth % pos) 10)) coll))

(defn- oxygen-generator-rating [coll]
  (let [bits (count (first coll))]
    (loop [coll coll
           pos 0]
      (if (or (> pos bits)
              (> (count coll) 1))
        (recur (by-val-and-pos coll pos (nth (gamma-rate coll) pos))
               (inc pos))
        (first coll)))))

(defn- co2-scrubber-rating [coll]
  (let [bits (count (first coll))]
    (loop [coll coll
           pos 0]
      (if (or (> pos bits)
              (> (count coll) 1))
        (recur (by-val-and-pos coll pos (nth (epsilon-rate coll) pos))
               (inc pos))
        (first coll)))))

(comment
  (def coll (parse mock-input))

  coll
  (gamma-rate coll)
  (by-val-and-pos coll 0 1)

  ()

  (oxygen-generator-rating coll)
  (co2-scrubber-rating coll))