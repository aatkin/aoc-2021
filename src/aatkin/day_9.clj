(ns aatkin.day-9
  (:require [clojure.string :as s]))

(def ^:private input (slurp "resources/input/day_9"))
(def ^:private mock-input (slurp "resources/mock_input/day_9"))

(defn- parse-row [s]
  (map #(Character/digit % 10) s))

(defn- parse [data]
  (->> (s/split-lines data)
       (map parse-row)))

(defn- low-point-two [n1 n2]
  (cond
    (every? some? [n1 n2]) (when (< n1 n2) n1)
    (some? n1) n1
    :else nil))
(defn- low-point-three [n1 n2 n3]
  (cond
    (every? some? [n1 n2 n3]) (when (and (< n2 n1) (< n2 n3)) n2)
    :else (or (low-point-two n2 n1)
              (low-point-two n2 n3))))
(defn- low-points [row]
  (flatten (concat [(apply low-point-two (take 2 row))]
                   (map (partial apply low-point-three) (partition 3 1 row))
                   [(apply low-point-two (reverse (take-last 2 row)))])))

(defn- two-row-low-points [row1 row2]
  (map low-point-two (low-points row1) row2))

(defn- three-row-low-points [row1 row2 row3]
  (map low-point-three row1 (low-points row2) row3))

(defn- all-low-points [data]
  (let [head-low-points (apply two-row-low-points (take 2 data))
        tail-low-points (apply two-row-low-points (reverse (take-last 2 data)))
        other-low-points (->> (partition 3 1 data)
                              (map (partial apply three-row-low-points)))]
    (conj (into [head-low-points] other-low-points) tail-low-points)))

(defn part-1-solution [data]
  (->> (all-low-points data)
       flatten
       (filter some?)
       (map inc)
       (apply +)))