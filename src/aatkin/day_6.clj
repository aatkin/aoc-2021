(ns aatkin.day-6
  (:require [clojure.string :as s]))

(def ^:private input (slurp "resources/input/day_6"))
(def ^:private mock-input (slurp "resources/mock_input/day_6"))

(defn- parse [input]
  (->> (s/split input #",")
       (map #(Integer/parseInt % 10))))

(defn- evolve-fishes [m]
  {0 (get m 1 0)
   1 (get m 2 0)
   2 (get m 3 0)
   3 (get m 4 0)
   4 (get m 5 0)
   5 (get m 6 0)
   6 (+ (get m 7 0) (get m 0 0))
   7 (get m 8 0)
   8 (get m 0 0)})

(defn- evolve [data n]
  (loop [t 0
         m (frequencies data)]
    (if (< t n)
      (recur (inc t)
             (evolve-fishes m))
      m)))

(defn part-1-solution []
  (let [fishes (-> (parse input)
                   (evolve 80)
                   vals)]
    (apply + fishes)))

(comment
  mock-input

  (parse mock-input)

  (get 3 (frequencies (parse mock-input)))

  (part-1-solution)
  )

(defn part-2-solution []
  (let [fishes (-> (parse input)
                   (evolve 256)
                   vals)]
    (apply + fishes)))

(comment
  (part-2-solution)
  )