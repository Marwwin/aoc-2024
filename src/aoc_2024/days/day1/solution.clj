(ns aoc-2024.days.day1.solution
  (:require
   [aoc-2024.utils :refer [load-data zip]]
   [clojure.string :as str]))

(defn part1 [[lefts rights]]
  (->> (zip (sort lefts) (sort rights))
       (reduce (fn [acc [left right]] (+ acc (abs (- left right)))) 0)))

(defn part2 [[lefts rights]]
  (let [rights-count (frequencies rights)]
    (->> lefts (reduce (fn [acc n] (+ acc (* n (get rights-count n 0)))) 0))))

(defn- parse-input [input]
  (loop [x      (first input)
         xs     (rest input)
         lefts  []
         rights []]
    (if-not (seq x)
      [lefts rights]
      (let [[left right] (str/split x #"   ")]
        (recur (first xs)
               (rest  xs)
               (conj  lefts (parse-long left))
               (conj  rights (parse-long right)))))))

(defn solve [current-ns input]
  (let [input (->> input
                   (load-data current-ns)
                   parse-input)]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

