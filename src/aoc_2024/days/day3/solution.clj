(ns aoc-2024.days.day3.solution
  (:require
   [aoc-2024.utils :refer [load-data log]]
   [clojure.string :as str]))

(defn count-mem [s]
  (->> s
       (re-seq #"mul\((\d+),(\d+)\)")
       (map (fn [[_ a b]] (* (parse-long a) (parse-long b))))
       (reduce + 0)))

(defn part1 [input]
  (->> input
       flatten
       str/join
       count-mem))

(defn part2 [input]
  (loop [memory   (-> input flatten (str/join))
         enabled? true
         result    0]
    (let [instruction            (if enabled? "don't()" "do()")
          offset                 (count instruction)
          next-instruction-index (str/index-of memory (if enabled? "don't()" "do()"))]
      (if (nil? next-instruction-index)
        (if enabled? (+ result (count-mem memory)) result)
        (let [chk           (subs memory 0 next-instruction-index)
              counts-in-chk (if enabled? (count-mem chk) 0)]
          (recur (subs memory (+ next-instruction-index offset)) (not enabled?) (+ counts-in-chk result)))))))

(defn solve [current-ns input-type]
  (let [input (load-data current-ns input-type)]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

