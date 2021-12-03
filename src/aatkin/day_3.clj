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
  (if (< 0.5 avg) 1 0))

(defn- least-common-bit [avg]
  (if (< 0.5 avg) 0 1))

(defn- gamma-rate [coll]
  (->> [0 1 2 3 4]
       (map #(get-nth-bits coll %))
       (map find-bit-average)
       (map most-common-bit)))

(defn- epsilon-rate [coll]
  (->> [0 1 2 3 4]
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
  (parse mock-input)

  (->> ((juxt gamma-rate epsilon-rate) (parse mock-input))
       (map to-integer)
       (apply *))
  )