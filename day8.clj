(ns day8
  (:require [clojure.string :as s]))

(def readlines (comp s/split-lines  slurp))

(defn parse-row [row]
  (vec (map (fn [c] (Integer/parseInt (str c))) row)))


(defn filter-by-last [pred coll]
  (reduce
   (fn [fnd x]
     (cond (empty? fnd) [x]
           (pred x (last fnd)) (conj fnd x)
           :else fnd))
   []
   coll))

(defn v+ [v1 v2]
  (vec (map + v1 v2)))

(defn neighbours-2d [[sr sc] [dr dc] array]
  (let [coords (iterate #(v+ [dr dc] %) [sr sc])]
    (take-while #(get-in array %) coords)))

(defn find-visible-trees-2d [pos dir array]
  (->> (neighbours-2d pos dir array)
       (filter-by-last #(> (get-in array %1) (get-in array %2)))))

(defn count-cols [array]
  (count (get array 0)))

(defn read-array [input]
  (->> (readlines input)
       (map parse-row)
       (vec)))

(defn count-from-edges [input]
  (let [forest (read-array input)
        left (map vector (range (count forest)) (repeat 0))
        right (map vector (range (count forest)) (repeat (dec (count-cols forest))))
        top (map vector (repeat 0) (range (count-cols forest)))
        bottom (map vector (repeat (dec (count forest))) (range (count-cols forest)))]
    (->> (concat
          (mapcat #(find-visible-trees-2d % [0 1] forest) left)
          (mapcat #(find-visible-trees-2d % [0 -1] forest) right)
          (mapcat #(find-visible-trees-2d % [1 0] forest) top)
          (mapcat #(find-visible-trees-2d % [-1 0] forest) bottom))
         (into #{})
         (count))))

(comment
  (count-from-edges "input8.txt")
  )


(defn take-until [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if ((complement pred) (first s))
       (cons (first s) (take-until pred (rest s)))
       (vector (first s))))))

(comment
  (take-until #(>= % 4) [1 2 3 1 4 5]))

(defn find-visible-from-above [pos dir array]
  (->> (neighbours-2d (v+ pos dir) dir array)
       (take-until #(>= (get-in array %1) (get-in array pos)))))


(defn scenic-score [pos forest]
  (let [vectors [[0 1] [0 -1] [1 0] [-1 0]]]
    (->> (map #(find-visible-from-above  pos % forest) vectors)
         (map count)
         (reduce *))))

(defn max-scenic-score [input]
  (let [arr (read-array input)]
    (->> (for [r (range (count arr))
               c (range (count-cols arr))]
           (scenic-score [r c] arr))
         (reduce max))))

(comment
  (scenic-score [1 2] (->> (readlines "example8.txt")
                           (map parse-row)
                           (vec)))

  (scenic-score [3 2] (->> (readlines "example8.txt")
                           (map parse-row)
                           (vec)))

  (max-scenic-score "example8.txt")
  (max-scenic-score "input8.txt")

  
  )



