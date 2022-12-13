(ns day9
  (:require [clojure.string :as s]))

(defn v+ [v1 v2]
  (vec (map + v1 v2)))

(defn v- [v1 v2]
  (vec (map - v1 v2)))

(defn dist [v1 v2]
  (apply max (mapv abs (v- v1 v2))))

(comment
  (dist [1 1] [1 1])
  (dist [1 1] [1 2])
  (dist [1 1] [2 2])
  (dist [1 1] [3 3]))

(defn follow [t h]
  (if (> (dist h t) 1)
    (->> (v- h t)
         (mapv #(max -1 (min 1 %))))
    [0 0]))

(comment
  (follow [1 1] [1 1])
  (follow [1 1] [2 1])

  (follow [1 1] [3 3])
  (follow [1 1] [4 5])
  (follow [1 1] [-2 1])
  (follow [1 1] [4 1]))

(defn move [[head & knots] dir]
  (when head
    (let [new-head (v+ head dir)
          new-dir (when (seq knots) 
                    (follow (first knots) new-head))]
      (cons new-head (move knots new-dir)))))

(comment
  (move [[1 1] [0 0]] [1 0])
  (move [[2 1] [1 1]] [1 0])
  (move [[3 1] [2 1]] [1 0])
  (move [[3 1] [2 1] [1 1]] [1 0])
  (cons 1 nil)

  )

(def readlines (comp s/split-lines  slurp))

(defn parse-line [line]
  (let [[_ d n] (re-matches  #"([LRUD]) ([0-9]+)" line)
        dir (case d
              "R" [1 0]
              "L" [-1 0]
              "U" [0 1]
              "D" [0 -1])]
    (repeat (parse-long n) dir)))

(defn count-visited [input knots]
  (->> (readlines input)
       (mapcat parse-line)
       (reductions move (repeat knots [0 0]))
       (map last)
       (into #{})
       (count)))

(comment
  (count-visited "example9.txt" 2)
  (time (count-visited "input9.txt" 2))

  (count-visited "example9.txt" 10)
  (count-visited "example9b.txt" 10)
  (time (count-visited "input9.txt" 10))

  )