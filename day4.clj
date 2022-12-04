(ns day4
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def readlines (comp s/split-lines  slurp))

(defn range-closed [a b] (range a (inc b)))

(defn parse-range [range-str]
  (->> (s/split range-str #"-")
       (map #(Integer/parseInt %))
       (apply range-closed)
       (into #{})))

(defn parse-pair [line]
  (->> (s/split line #",")
       (map parse-range)))

(defn mutual-subsets? [a b]
  (or (set/subset? a b)
      (set/subset? b a)))

(defn overlap? [s1 s2]
  (not-empty (set/intersection s1 s2)))

(defn count-range-pairs [input pred]
  (->> (readlines input)
       (map parse-pair)
       (filter (partial apply pred))
       (count)))

(defn count-mutual-subsets [input]
  (count-range-pairs input mutual-subsets?))

(defn count-overlapping [input]
  (count-range-pairs input overlap?))

(comment
  (count-mutual-subsets "example4.txt")
  (count-mutual-subsets "input4.txt")

  (count-overlapping "example4.txt")
  (count-overlapping "input4.txt"))