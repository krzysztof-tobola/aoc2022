(ns day11
  (:require [clojure.string :as s]))


(def readlines (comp s/split-lines  slurp))

(defn parse-throw [line]
  (parse-long (get (re-matches #".*throw to monkey (\d+)" line) 1)))

(defn parse-operation [line]
  (let [[_ lv op rv] (re-matches #"  Operation: new = (\w+) (.) (\w+)" line)
        f (case op
            "*" *'
            "+" +'
            "/" quot
            "-" -')
        ll (parse-long lv)
        rl (parse-long rv)]
    (fn [x] (f (or ll x) (or rl x)))))

(comment
  (parse-long "3")
  (parse-long "x"))

(defn parse-monkey [[id items op test pass fail]]
  {:id (parse-long (get (re-matches #"Monkey (\d+):" id) 1))
   :items (->> (re-seq #"\d+" items) (map read-string))
   :operation  (parse-operation op)
   :op-raw op
   :test-div-by (read-string (get (re-matches #"  Test: divisible by (\d+)" test) 1))
   :pass (parse-throw pass)
   :fail (parse-throw fail)
   :insp-count 0})

(defn parse-monkeys [input]
  (->> (readlines input)
       (partition-by s/blank?)
       (remove #(= [""] %))
       (map parse-monkey)
       (vec)))

(defn div3 [x]  (quot x 3))

(defn update-monkey [worryfn modulo state id]
  (let [monkey    (get state id)
        inspected (-> monkey
                      (update :items #(map (comp (fn [x] (mod x modulo))
                                                 worryfn
                                                 (:operation monkey)) %))
                      (update :insp-count + (count (:items monkey))))
        updated   (assoc inspected :items [])
        test-results (group-by #(zero? (rem % (:test-div-by inspected))) (:items inspected))]

    (-> state
        (assoc id updated)
        (update-in  [(:pass monkey) :items] concat (get test-results true))
        (update-in  [(:fail monkey) :items] concat (get test-results false)))))


(defn round [worryf modulo state]
  (reduce  #(update-monkey worryf modulo %1 %2) state (range (count state))))

(defn do-rounds [worryf rounds input]
  (let [state (parse-monkeys input)
        modulo (reduce * (map :test-div-by state))
        _ (println "modulo" modulo)]
    (nth  (iterate (partial round worryf modulo) state)
          rounds)))


(defn monkey-business [worryf rounds input]
  (->>  (do-rounds worryf rounds input)
        (sort-by  (comp - :insp-count))
        (take 2)
        (map :insp-count)
        (reduce *')))

(comment
  (do
    (assert (= 10605 (monkey-business div3 20 "example11.txt")))
    (assert (= 119715 (monkey-business div3 20 "input11.txt"))))


  (time (monkey-business identity 10000 "example11.txt"))
  (time (monkey-business identity 10000 "input11.txt"))
  
  )