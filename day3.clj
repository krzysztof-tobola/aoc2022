(ns day3
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn split-in-half [s] (split-at (/ (count s) 2) s))

(defn overlapping-items [coll]
  (->> (map set coll)
       (reduce set/intersection)))

(def ALPHABET "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn priority [item] (inc (s/index-of ALPHABET item)))

(def readlines (comp s/split-lines  slurp))

(defn sum-overlapping-priorities [partfn input]
  (->> (readlines input)
       (partfn)
       (mapcat overlapping-items)
       (map priority)
       (reduce +)))

(defn badge-priorities [input]
  (sum-overlapping-priorities  #(partition 3 %) input))

(defn reordering-priorities [input]
  (sum-overlapping-priorities  #(map split-in-half %) input))

(comment
  (badge-priorities "example3.txt")
  (badge-priorities "input3.txt")

  (reordering-priorities "example3.txt")
  (reordering-priorities "input3.txt"))