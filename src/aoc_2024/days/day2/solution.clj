(ns aoc-2024.days.day2.solution
  (:require
   [aoc-2024.utils :refer [load-data]]
   [clojure.string :as str]))

(defn safe-level? [level]
  (let [level-ascending? (< (first level) (second level))]
    (loop [xs level]
      (if (nil? (second xs))
        true
        (let [distance (if level-ascending?
                         (- (second xs) (first xs))
                         (- (first xs) (second xs)))]
          (if (or (<= distance 0)
                  (> distance 3))
            false
            (recur (rest xs))))))))

(defn part1 [input]
  (->> input
       (reduce
        (fn [acc level]
          (if (safe-level? level) (inc acc) acc)) 0)))

(defn remove-by-index [coll i]
  (if (>= i (count coll))
    coll
    (let [coll (vec coll)]
      (concat (subvec coll 0 i)
              (subvec coll (inc i))))))

(defn part2 [input]
  (->> input
       (reduce
        (fn [acc level]
          (loop [xs level
                 i 0]
            (if (> i (count level))
              acc
              (if (safe-level? xs)
                (inc acc)
                (recur (remove-by-index level i) (inc i)))))) 0)))

(defn- parse-input [input]
  (->> input
       (map (fn [row] (map parse-long (str/split row #" "))))))

(defn solve [current-ns input-type]
  (let [input (-> (load-data current-ns input-type)
                  parse-input)]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

