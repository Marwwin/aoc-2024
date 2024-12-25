(ns aoc-2024.days.day25.solution
  (:require
   [aoc-2024.utils :refer [load-data]]))

(defn transpose [a]
  (apply mapv vector a))

(defn count-pins [schema]
  (->> schema
       (map transpose)
       (mapv (fn [a] (map #(dec (count (filter #{\#} %))) a)))))

(defn part1 [input]
  (let [{:keys [locks keys]} (loop [lines input
                                    schema []
                                    result {:lock [] :key []}]
                               (cond
                                 (empty? lines)         result
                                 (empty? (first lines)) (recur (rest lines) schema result)
                                 (= (count schema) 6)   (recur (rest lines) [] (if (every? #{\#} (first schema))
                                                                                 (update result :locks conj (conj schema (first lines)))
                                                                                 (update result :keys conj (conj schema (first lines)))))
                                 :else
                                 (recur (rest lines) (conj  schema (first lines)) result)))
        locks                (count-pins locks)
        keys                 (count-pins keys)]

    (->> (for [key keys
               lock locks
               :when (every? #(< % 6) (map + key lock))]
           true)
         count)))

(defn part2 [input])

(defn solve [current-ns input-type]
  (let [input (load-data current-ns input-type)]
    (println (first input))
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

