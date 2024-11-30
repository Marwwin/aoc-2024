(ns aoc-2024.days.day1.solution
  (:require
   [aoc-2024.utils :refer [load-data]]))

(defn part1 [input])

(defn part2 [input])

(defn solve [current-ns input]
  (let [input (load-data current-ns input)]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
; (solve (str *ns*) :input)

