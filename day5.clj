(ns day5
  (:require [clojure.string :as s]))

(defn parse-stack-line [line]
  (->> (partition-all 4 line)
       (map #(apply str %))
       (map #(s/replace % #"[\[\] ]" ""))))

(comment
  (parse-stack-line "[N] [C]    "))

(defn column [t n]
  (map #(nth % n) t))

(defn translate-table [t]
  (let [cols (count (first t))]
    (->> (map #(column t %) (range cols)))))

(defn parse-move-line [line]
  (let [[n from to]
        (->> (re-matches #"move (\d+) from (\d+) to (\d+)" line)
             (rest)
             (map #(Integer/parseInt %)))]
    {:n n :from (dec from) :to (dec to)}))

(defn apply-move [modifier stacks {:keys [n from to]}]
  (let [moved (take n (get stacks from))]
    (-> stacks
        (update from  #(drop n %))
        (update to  #(concat  (modifier moved) %)))))

(comment
  (apply-move reverse
              [["N" "Z"] ["D" "C" "M"] ["P"]]
              {:n 2 :from 0 :to 2})
  (apply-move identity
              [["N" "Z"] ["D" "C" "M"] ["P"]]
              {:n 2 :from 0 :to 2}))

(defn parse-stacks [raw-stacks]
  (->> (map parse-stack-line raw-stacks)
       (translate-table)
       (map drop-last)
       (map #(remove empty? %))
       (map vec)
       (into [])))

(defn parse [input]
  (let [[stack-lines move-lines]
        (->> (s/split (slurp input) #"\n\n")
             (map s/split-lines))]
    [(parse-stacks stack-lines)
     (map parse-move-line move-lines)]))

(comment
  (parse "example5.txt")
  )

(defn rearrange-crates [input modify-crates]
  (let [[stacks moves] (parse input)]
    (->> (reduce (partial apply-move modify-crates)
                 stacks
                 moves)
         (map first)
         (apply str))))

(comment
  (rearrange-crates "example5.txt" reverse)
  (rearrange-crates "input5.txt" reverse)

  (rearrange-crates "example5.txt" identity)
  (rearrange-crates "input5.txt" identity))