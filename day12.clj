(ns day12
  (:require [clojure.string :as s]))

(def readlines (comp s/split-lines  slurp))

(defn correct-order? [items]
  (= items (sort items)))

(comment
  (read-string "") 
  
  (->> (readlines "example12.txt")
       (remove s/blank?)
       (map read-string)
       (partition 2)
       (map correct-order?))
  
  )