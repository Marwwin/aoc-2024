(ns aoc-2024.day-template
  (:require
   [aoc-2024.utils :refer [load-data]]))

(defn solve [current-ns input]
  (let [input (load-data current-ns input)]
    (println "hello i am solve")
    (println input)))

;; For repl
; (solve (str *ns*) :input)

(defn part1 [])

(defn part2 [])
