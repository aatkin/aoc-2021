(ns aatkin.day-5
  (:require [clojure.string :as s]))

(def ^:private input (slurp "resources/input/day_5"))
(def ^:private mock-input (slurp "resources/mock_input/day_5"))

(defn- parse-coord [s]
  (let [[x y] (s/split s #",")]
    [(Integer/parseInt x 10) (Integer/parseInt y 10)]))

(defn- parse-coords [in]
  (->> (s/split-lines in)
       (map #(s/split % #" -> "))
       (map (fn [[from to]]
              [(parse-coord from) (parse-coord to)]))))

(defn- not-diagonal? [[from to]]
  (or (= (first from) (first to))
      (= (second from) (second to))))

(defn- plot-range [from to]
  (if (< to from)
    (range from (dec to) -1)
    (range from (inc to))))

(defn- plot-range-diagonal [[x1 x2] [y1 y2]]
  (->> (interleave (plot-range x1 x2)
                   (plot-range y1 y2))
       (partition 2)))

(defn- plot-path [[[x1 y1] [x2 y2]]]
  (if (= x1 x2)
    (map #(list x1 %) (plot-range y1 y2))
    (if (= y1 y2)
      (map #(list % y1) (plot-range x1 x2))
      (map #(list (first %) (second %)) (plot-range-diagonal [x1 x2] [y1 y2])))))

(defn- plot-paths [data]
  (loop [data data
         paths {}]
    (if (seq data)
      (recur (rest data)
             (reduce (fn [m coord] (assoc m coord (inc (get m coord 0))))
                     paths
                     (plot-path (first data))))
      paths)))

(defn part-1-solution []
  (->> (parse-coords input)
       (filter not-diagonal?)
       plot-paths
       (filter #(> (second %) 1))
       count))

(defn part-2-solution []
  (->> (parse-coords input)
       plot-paths
       (filter #(> (second %) 1))
       count))

(comment
  (plot-range-diagonal [1 1] [3 3])

  (part-1-solution)

  (part-2-solution)
  )