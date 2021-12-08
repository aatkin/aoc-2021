(ns aatkin.day-8
  (:require [clojure.string :as s]
            [clojure.set :refer [difference union]]))

(def ^:private input (slurp "resources/input/day_8"))
(def ^:private mock-input (slurp "resources/mock_input/day_8"))

(defn- parse-codes [line]
  (->> (s/split line #" \| ")
       (map #(s/split % #" "))))

(defn- parse [data]
  (->> (s/split-lines data)
       (map parse-codes)))

(defn- count-simple-codes [data]
  (let [counts (-> (map count data)
                   frequencies
                   (select-keys [2 3 4 7])
                   vals)]
    (apply + counts)))

(defn- part-1-solution []
  (->> (parse input)
       (map second)
       (map count-simple-codes)
       (apply +)))

(defn- mask [s1 s2]
  (union (set s1) (set s2)))

(defn- decode-n [known coll {:keys [n with-mask eq filter-fn]}]
  (let [filter-fn (or filter-fn identity)]
    (->> (difference (set coll) (set (vals known))) ;; remove known codes
         filter-fn ;; optional filtering (e.g. only 6 len codes)
         (filter #(= (mask (get known with-mask) %)
                     (set (get known eq))))
         first
         (assoc known n))))

(defn- take-one [known coll {:keys [n filter-fn]}]
  (let [filter-fn (or filter-fn identity)]
    (assoc known n (->> (difference (set coll) (set (vals known)))
                        filter-fn
                        first))))

(defn- codes-by-len [n coll]
  (filter #(= n (count %)) coll))

(defn- decode-known [coll]
  (let [known (-> {1 (first (codes-by-len 2 coll))
                   4 (first (codes-by-len 4 coll))
                   7 (first (codes-by-len 3 coll))
                   8 (first (codes-by-len 7 coll))}
                  (decode-n coll {:n 6 :with-mask 1 :eq 8})
                  (decode-n coll {:n 0 :with-mask 4 :eq 8 :filter-fn (partial codes-by-len 6)})
                  (decode-n coll {:n 5 :with-mask 6 :eq 6})
                  (take-one coll {:n 9 :filter-fn (partial codes-by-len 6)})
                  (decode-n coll {:n 3 :with-mask 9 :eq 9})
                  (take-one coll {:n 2}))]
    ;; swap keys <-> values
    (reduce-kv #(assoc %1 (s/join (sort %3)) %2) {} known)))

(defn- decode [[codes digits]]
  (let [known (decode-known codes)]
    (map #(get known (s/join (sort %))) digits)))

(defn- to-int [decoded]
  (Integer/parseInt (s/join decoded) 10))

(defn part-2-solution []
  (->> (parse input)
       (map decode)
       (map to-int)
       (apply +)))