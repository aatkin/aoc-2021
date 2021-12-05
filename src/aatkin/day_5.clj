(ns aatkin.day-5
  (:require [clojure.string :as s]))

(def ^:private input (slurp "resources/input/day_5"))
(def ^:private mock-input (slurp "resources/mock_input/day_5"))

(defn- parse-coords [s]
  (let [[x y] (s/split s #",")]
    [(Integer/parseInt x 10) (Integer/parseInt y 10)]))

(defn- parse-part-1 [input]
  (->> (s/split-lines input)
       (map #(s/split % #" -> "))
       (map (fn [[from to]]
              [(parse-coords from) (parse-coords to)]))
       (filter (fn [[from to]]
                 (or (= (first from) (first to))
                     (= (second from) (second to)))))))

(defn- parse-part-2 [input]
  (->> (s/split-lines input)
       (map #(s/split % #" -> "))
       (map (fn [[from to]]
              [(parse-coords from) (parse-coords to)]))))

(defn- plot-range [from to]
  (if (< to from)
    (range from (dec to) -1)
    (range from (inc to))))

(defn- plot-range-diagonal [[from-x to-x] [from-y to-y]]
  (->> (interleave (plot-range from-x to-x)
                   (plot-range from-y to-y))
       (partition 2)))

(defn- plot-path [[[from-x from-y] [to-x to-y]]]
  (if (= from-x to-x) ;; x1 = x2
    (map #(list from-x %) (plot-range from-y to-y))
    (if (= from-y to-y) ;; y1 = y2
      (map #(list % from-y) (plot-range from-x to-x))
      (map #(list (first %) (second %)) ;; diagonal
           (plot-range-diagonal [from-x to-x] [from-y to-y])))))

(defn- plot-paths [data]
  (loop [data data
         paths {}]
    (if (seq data)
      (recur (rest data)
             (reduce (fn [m coord-point]
                       (assoc m coord-point
                              (inc (get m coord-point 0))))
                     paths
                     (plot-path (first data))))
      paths)))

(defn part-1-solution []
  (->> (parse-part-1 input) ;; ignore diagonal paths
       plot-paths
       (filter #(> (second %) 1))
       count))

(defn part-2-solution []
  (->> (parse-part-2 input) ;; include diagonal paths
       plot-paths
       (filter #(> (second %) 1))
       count))

(comment
  (plot-range-diagonal [1 1] [3 3])

  (part-1-solution)

  (part-2-solution)
  )