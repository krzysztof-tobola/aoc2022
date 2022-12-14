(ns day10
  (:require [clojure.string :as s]))

(def readlines (comp s/split-lines  slurp))

(defn parse-line [line]
  (condp re-matches line
    #"addx (-?[0-9]+)" :>> (fn [[_ i]] [{:cmd :noop}
                                        {:cmd :add :x (parse-long i)}])
    [{:cmd :noop}]))


(defn apply-cmd [x cmd]
  (case (:cmd cmd)
    :noop x
    :add  (+ x (:x cmd))))

(comment
  (apply-cmd 1 {:cmd :noop})
  (apply-cmd 1 {:cmd :add :x 4})
  (apply-cmd 1 {:cmd :add :x -4}))

(defn read-signal [input]
  (->> (readlines input)
       (mapcat parse-line)
       (reductions apply-cmd 1)
       (into [])))

(defn signal-strenghts [input]
  (let [signal (vec (read-signal input))]
    (->> (map #(* % (get signal (dec %))) [20 60 100 140 180 220])
         (reduce +))))

(defn render-pixel [x sprite]
  (if (< (abs (- x sprite)) 2) "#" "."))

(defn render-line [line]
  (s/join (map-indexed render-pixel line)))

(defn render-signal [input]
  (->> (read-signal input)
       (partition 40)
       (map render-line)))

(comment
  (signal-strenghts "example10.txt")
  (signal-strenghts "input10.txt")

  (render-signal "example10.txt")
  (render-signal "input10.txt"))