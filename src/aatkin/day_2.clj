(ns aatkin.day-2
  (:require [clojure.string :as s]))

(def ^:private input (slurp "resources/input/day_2"))
(def ^:private mock-input (slurp "resources/mock_input/day_2"))

(defn- parse [input]
  (->> (s/split-lines input)
       (map #(s/split % #" "))
       (map (fn [[type value]] [type (Integer/parseInt value)]))))

(defn- horizontal-pos? [[type]]
  (= "forward" type))
(defn- depth-value [[type n]]
  (if (= "up" type) (- n) n))

(defn- get-final-position [input]
  (let [horizontals (map second (filter horizontal-pos? input))
        depths (map depth-value (remove horizontal-pos? input))]
    (* (apply + horizontals)
       (apply + depths))))

(defn part-1-solution []
  (get-final-position (parse input)))

(comment
  (parse mock-input)
  (get-final-position (parse mock-input))
  (get-final-position (parse input))
  )

(defn- calculate-aim [[type n]]
  (condp = type
    "down" n
    "up" (- n)
    0))

(defn- calculate-horizontal [[type n]]
  (if (= type "forward")
    n 0))

(defn- calculate-depth [[type n] aim]
  (if (= type "forward")
     (* n aim) 0))

(defn- get-final-position-corrected [values]
  (loop [values values
         aim 0
         horizontal 0
         depth 0]
    (if-let [current (first values)]
      (recur (rest values)
             (+ aim (calculate-aim current))
             (+ horizontal (calculate-horizontal current))
             (+ depth (calculate-depth current aim)))
      (* horizontal depth))))

(defn part-2-solution []
  (get-final-position-corrected (parse input)))

(comment
  (parse mock-input)
  (get-final-position-corrected (parse mock-input))
  (get-final-position-corrected (parse input))
  )