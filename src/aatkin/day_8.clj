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

(defn- matches? [s1 s2]
  (fn [s3] (= (mask s1 s3) (set s2))))

(defn- find-match [coll s1 s2]
  (first (filter (matches? s1 s2) coll)))

(defn- codes-by-len [n coll]
  (filter #(= n (count %)) coll))

(defn- without-known [coll known]
  (difference (set coll) (set (vals known))))

(defn- set-match! [known n coll s1 s2]
  (when-let [match (find-match (without-known coll @known) s1 s2)]
    (reset! known (assoc @known n match))))

(defn- sort-str [s]
  (s/join (sort s)))

(defn- find-known [coll]
  (let [known (atom {1 (first (codes-by-len 2 coll))
                     4 (first (codes-by-len 4 coll))
                     7 (first (codes-by-len 3 coll))
                     8 (first (codes-by-len 7 coll))})]
    ;; 1 & 6 = 8
    (set-match! known 6 coll (get @known 1) (get @known 8))
    ;; 0 & 4 = 8
    (set-match! known 0 (codes-by-len 6 coll) (get @known 4) (get @known 8))
    ;; 5 & 6 = 6
    (set-match! known 5 coll (get @known 6) (get @known 6))
    ;; only 9 left out of all 6 len codes
    (reset! known (assoc @known 9 (first (without-known (codes-by-len 6 coll) @known))))
    ;; only 2 and 3 left, 3 & 9 = 9
    (set-match! known 3 coll (get @known 9) (get @known 9))
    ;; last match left
    (reset! known (assoc @known 2 (first (without-known coll @known))))

    ;; vals to keys
    (reduce-kv #(assoc %1 (sort-str %3) %2) {} @known)))

(defn- decode [[codes digits]]
  (let [known (find-known codes)]
    (map (fn [code]
           (get known (sort-str code))) digits)))

(defn- to-int [decoded]
  (Integer/parseInt (s/join decoded) 10))

(defn part-2-solution []
  (->> (parse input)
       (map decode)
       (map to-int)
       (apply +)))