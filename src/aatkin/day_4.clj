(ns aatkin.day-4
  (:require [clojure.string :as s]
            [clojure.set :refer [difference]]))

(def ^:private input (slurp "resources/input/day_4"))
(def ^:private mock-input (slurp "resources/mock_input/day_4"))

(defn- to-int [s] (Integer/parseInt s 10))
(defn- split-csv [s] (s/split s #","))
(defn- parse-table-row [str]
  (let [formatted (-> (s/trim str)
                      (s/replace #"\s+" ","))]
    (map to-int (split-csv formatted))))

(defn- parse [input]
  (let [data (s/split-lines input)
        draw-numbers (map to-int (split-csv (first data)))]
    (loop [data (drop 2 data)
           tables []]
      (if (seq data)
        (recur (drop 6 data)
               (conj tables (map parse-table-row (take 5 data))))
        [draw-numbers tables]))))

(defn- transpose [coll] (apply map vector coll))

(defn- get-marked-numbers [n row]
  (into (:marked (meta row) #{})
        (filter #(= % n) row)))

(defn- mark-table [n table]
  (let [mark-row #(with-meta % {:marked (get-marked-numbers n %)})
        marked-columns (->> (:marked-columns (meta table) (transpose table))
                            (map mark-row))]
    (-> (map mark-row table)
        (with-meta {:marked-columns marked-columns}))))

(defn- row-bingo? [row]
  (= (count row)
     (count (:marked (meta row)))))

(defn- get-unmarked [table]
  (let [all-vals (flatten table)
        marked (->> (map (comp :marked meta) table)
                    (reduce #(into %1 %2) #{}))]
    (difference (set all-vals) marked)))

(defn- bingo-value [n table]
  (let [rows (map row-bingo? table)
        cols (map row-bingo? (:marked-columns (meta table)))
        found (->> (into rows cols)
                   (filter true?))]
    (when (seq found)
      (* (apply + (get-unmarked table)) n))))

(defn- find-bingo [draw-numbers tables]
  (loop [draw-numbers draw-numbers
         tables tables
         bingo-val nil]
    (if bingo-val
      bingo-val
      (when-let [n (first draw-numbers)]
        (let [marked-tables (map (partial mark-table n) tables)]
          (recur (rest draw-numbers)
                 marked-tables
                 (->> (map (partial bingo-value n) marked-tables)
                      (filter some?)
                      first)))))))

(defn part-1-solution []
  (apply find-bingo (parse input)))

(defn- find-last-bingo [draw-numbers tables]
  (loop [draw-numbers draw-numbers
         tables tables
         bingo-val nil]
    (if (empty? tables)
      bingo-val
      (when-let [n (first draw-numbers)]
        (let [new-tables (map (partial mark-table n) tables)]
          (recur (rest draw-numbers)
                 (remove (partial bingo-value n) new-tables)
                 (bingo-value n (first new-tables))))))))

(defn part-2-solution []
  (apply find-last-bingo (parse input)))

(comment
  (def data (parse mock-input))

  (apply find-bingo (parse mock-input))

  (apply find-last-bingo (parse mock-input))

  (part-1-solution)

  (part-2-solution))