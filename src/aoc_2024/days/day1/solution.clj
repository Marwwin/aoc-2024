(ns aoc-2024.days.day1.solution
  (:require
   [aoc-2024.utils :refer [load-data]]))

(defn solve [input]
  (let [input (load-data (str *ns*) input)]
    (println input (count input))
    (println "hello i am solve")))

(solve :test)

(defn part1 [])

(defn part2 [])

