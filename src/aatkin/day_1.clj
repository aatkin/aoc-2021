(ns aatkin.day-1
  (:require [clojure.string :as s]))

(def ^:private input (slurp "resources/input/day_1"))
(def ^:private mock-input (slurp "resources/mock_input/day_1"))

(defn- parse [input]
  (->> (s/split-lines input)
       (map #(Integer/parseInt %))))

(defn- measurement-diffs [data]
  (->> (partition 2 1 data)
       (map #(< (first %) (second %)))
       (filter true?)
       count))

(comment
  (measurement-diffs (parse input))
  (measurement-diffs (parse mock-input)))

(defn part-1-solution []
  (measurement-diffs (parse input)))

(defn- sliding-window-diffs [data]
  (->> (partition 3 1 data)
       (map (partial apply +))
       measurement-diffs))

(comment
  (sliding-window-diffs (parse mock-input))
  (sliding-window-diffs (parse input)))

(defn part-2-solution []
  (sliding-window-diffs (parse input)))