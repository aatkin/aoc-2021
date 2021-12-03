(ns aatkin.day-3
  (:require [clojure.string :as s]))

(def ^:private input (slurp "resources/input/day_3"))
(def ^:private mock-input (slurp "resources/mock_input/day_3"))

(defn- parse [input] (s/split-lines input))

(defn- transpose [coll] (apply map vector coll))

(defn- bin-to-int [str]
  (Integer/parseInt str 2))
(defn- char-to-int [c] (Character/digit c 10))
(defn- chars-to-ints [coll] (map char-to-int coll))

(defn- find-bit-averages [coll]
  (let [bit-sums (->> (map chars-to-ints (transpose coll))
                      (map #(apply + %)))
        avg #(/ % (count coll))]
    (map avg bit-sums)))

(defn- most-common-bit [avg]
  (if (<= 0.5 avg) 1 0))
(defn- least-common-bit [avg]
  (if (<= 0.5 avg) 0 1))

(defn- gamma-rate [coll]
  (->> (find-bit-averages coll)
       (map most-common-bit)))

(defn- epsilon-rate [coll]
  (->> (find-bit-averages coll)
       (map least-common-bit)))

(defn part-1-solution []
  (->> ((juxt gamma-rate epsilon-rate) (parse input))
       (map (comp bin-to-int s/join))
       (apply *)))

(comment
  (->> ((juxt gamma-rate epsilon-rate) (parse mock-input))
       (map (comp bin-to-int s/join))
       (apply *))
  
  (part-1-solution)
  )

(defn- by-val-and-pos [coll pos n]
  (filter #(= n (char-to-int (nth % pos))) coll))

(defn- find-value-by [f coll]
  (let [bits (count (first coll))]
    (loop [coll coll
           pos 0]
      (if (or (> pos bits)
              (> (count coll) 1))
        (recur (by-val-and-pos coll pos (nth (f coll) pos))
               (inc pos))
        (first coll)))))
(def ^:private oxygen-generator-rating (partial find-value-by gamma-rate))
(def ^:private co2-scrubber-rating (partial find-value-by epsilon-rate))

(defn part-2-solution []
  (->> ((juxt oxygen-generator-rating co2-scrubber-rating) (parse input))
       (map (comp bin-to-int s/join))
       (apply *)))

(comment
  (->> ((juxt oxygen-generator-rating co2-scrubber-rating) (parse mock-input))
       (map (comp bin-to-int s/join))
       (apply *))

  (part-2-solution)
  )