(ns aatkin.day-7
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :refer [abs]]))

(def ^:private input (slurp "resources/input/day_7"))
(def ^:private mock-input (slurp "resources/mock_input/day_7"))

(defn- parse [input]
  (->> (s/split input #",")
       (map #(Integer/parseInt % 10))))

(defn- distance [a b]
  (abs (- a b)))

(defn- median [coll]
  (let [idx (Math/floor (/ (count coll) 2))]
    (nth (sort coll) idx)))

(defn- least-fuel [data]
  (->> data
       (map (partial distance (median data)))
       (apply +)))

(defn part-1-solution []
  (->> (parse input)
       least-fuel))

(defn- avg [data]
  (/ (apply + data)
     (count data)))

(defn- sum-up-to [n]
  (->> (range 1 (+ n 1))
       (apply +)))

(defn- least-fuel-by [data rounding-fn]
  (->> data
       (map (partial distance (rounding-fn (avg data))))
       (map sum-up-to)
       (apply +)))

(defn part-2-solution []
  (let [data (parse input)]
    (Math/min (least-fuel-by data #(Math/floor %))
              (least-fuel-by data #(Math/ceil %)))))