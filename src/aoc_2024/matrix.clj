(ns aoc-2024.matrix
  (:require
   [aoc-2024.utils :as utils]))

(defn create [xs]
  (-> xs
      utils/log)
  ())


(create '("1 2 3" "4 5 6" "7 8 9"))

