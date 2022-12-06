(ns day6
  (:require [clojure.string :as s]))

(def readlines (comp s/split-lines slurp))

(defn find-distinct-marker [len s]
  (->> (partition len 1 s)
       (map-indexed #(hash-map  :offset %1 :code (set %2)))
       (filter (comp #{len} count :code))
       (first)
       (:offset)
       (+ len)))

(comment 
  (find-distinct-marker 4 (slurp "input6.txt"))
  (find-distinct-marker 14 (slurp "input6.txt"))

  (->> (readlines "example6.txt")
       (map (partial find-distinct-marker 14)))
  )