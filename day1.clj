(ns day1
  (:require [clojure.string :as s]))

(defn parse [s]
  (if (s/blank? s) nil (Integer/valueOf s)))

(defn parse-input [input]
  (->> (s/split-lines (slurp input))
       (map parse)
       (partition-by nil?)
       (remove #(= [nil] %))))

(defn top-capacities [packs]
  (->> (map (partial apply +) packs)
       (sort-by -)))

(defn max-capacity [packs]
  (->> (top-capacities packs)
       (apply max)))

(defn top3-capacity [packs]
  (->> (top-capacities packs)
       (take 3)
       (apply +)))

(comment
  (->> (parse-input "example1.txt")
       (max-capacity))

  (->> (parse-input "example1.txt")
       (top3-capacity))

  (->> (parse-input "input1.txt")
       (max-capacity))

  (->> (parse-input "input1.txt")
       (top3-capacity))

  
  )