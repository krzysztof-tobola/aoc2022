(ns day2
  (:require [clojure.string :as s]))

(def scores
  {"A X" (+ 1 3)
   "A Y" (+ 2 6)
   "A Z" (+ 3 0)
   "B X" (+ 1 0)
   "B Y" (+ 2 3)
   "B Z" (+ 3 6)
   "C X" (+ 1 6)
   "C Y" (+ 2 0)
   "C Z" (+ 3 3)})


(def scores-2
  ;a-rock-1 b-paper-2 c-scissors-3
  ;x-lose-0 y-draw-3 z-win-6
  {"A X" (+ 0 3)
   "A Y" (+ 3 1)
   "A Z" (+ 6 2)
   "B X" (+ 0 1)
   "B Y" (+ 3 2)
   "B Z" (+ 6 3)
   "C X" (+ 0 2)
   "C Y" (+ 3 3)
   "C Z" (+ 6 1)})

(defn count-score [input scores]
  (->> (slurp input)
       (s/split-lines)
       (map scores)
       (reduce +)))

(comment
  (count-score "example2.txt" scores)
  (count-score "input2.txt" scores)

  (count-score "example2.txt" scores-2)
  (count-score "input2.txt" scores-2)

  )