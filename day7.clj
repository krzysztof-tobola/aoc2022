(ns day7
  (:require [clojure.string :as s]))

(def readlines (comp s/split-lines  slurp))

(defn parse-event [l]
  (condp  re-matches l
    #"\$ cd /"  {:type :cdroot}
    #"\$ cd .."  {:type :cdup}
    #"\$ cd (.*)" :>> (fn [[_ dir]] {:type :cd :dir dir})
    #"\$ ls" {:type :ls}
    #"dir (.*)" :>> (fn [[_ name]] {:type :dir :name name})
    #"([0-9]+) (.*)" :>> (fn [[_ size name]] {:type :file :name name
                                              :size (Integer/parseInt size)})
    #".*" :unknown))

(comment
  (parse-event "$ cd /")
  (parse-event "$ cd a")
  (parse-event "$ ls")
  (parse-event "dir a")
  (parse-event "14848514 b.txt")
  )

(defn apply-event [state op]
  (-> (case (:type op)
        :cdroot (assoc state :path [])
        :cdup (update state :path butlast)
        :cd (update state :path conj (:dir op))
        (:dir, :file)  (assoc-in state
                                 (concat [:fs :children] (interpose :children (concat (:path state) [(:name op)])))
                                 {:name (:name op) :size (:size op)})
        state)
      (update :path vec)))

(comment
  (->> (map parse-event
            ["$ cd /"
             "$ cd a"
             "dir b"
             "$ cd b"
             "33 x.jpg"
             "$ ls"
             "dir trash"
             "1234 a.cr2"
             "$ cd .."
             "$ cd .."
             "$ cd c"
             "dir bla"
             "123 a.txt"])
       (reduce apply-event {}))
  )

(defn dir-size [dir]
  (if (empty? (:children dir))
    (:size dir)
    (->> (map dir-size (vals (:children dir)))
         (reduce +))))

(comment
  (dir-size {:size 123})
  (dir-size {:children {"a" {:size 123}
                        "b" {:size 7}}})
  )

(defn read-fs [input]
  (->> (readlines input)
       (map parse-event)
       (reduce apply-event {})
       :fs))

(defn dir-sizes [root]
  (->> (tree-seq :children (comp vals :children) root)
       (filter :children)
       (map dir-size)))

(defn part-1 [input]
  (->> (read-fs input)
       (dir-sizes)
       (filter #(<= % 100000))
       (reduce +)))

(def total-space 70000000)
(def min-free 30000000)

(defn part-2 [input]
  (let [root (read-fs input)
        free (- total-space (dir-size root))]
    (->> (dir-sizes root)
         (filter #(>= (+ free %) min-free))
         (reduce min))))

(comment
  (part-2 "example7.txt")
  (part-2 "input7.txt")
  
  (part-1 "example7.txt")
  (part-1 "input7.txt")
  
  )