(ns day1
  (:require [clojure.string :as s]))

(def example-input
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn parse [s]
  (if (s/blank? s) nil (Integer/valueOf s)))

(defn parse-input [input]
  (->>
   (s/split-lines input)
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
       (take 3)))

(comment
  (->> (parse-input example-input)
       (max-capacity))

  (->> (parse-input (slurp "input1.txt"))
       (max-capacity))

  (->> (parse-input (slurp "input1.txt"))
       (top3-capacity)
       (apply +))

  (->> (parse-input example-input)
       (top3-capacity)
       (apply +))

  )