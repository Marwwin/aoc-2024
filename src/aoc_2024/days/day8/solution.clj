(ns aoc-2024.days.day8.solution
  (:require
   [aoc-2024.matrix :as matrix]
   [aoc-2024.utils :refer [load-data log]]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]))

(defn create-antinodes [ain]
  (let [a (first ain)
        b (second ain)]
    [{:x (+ (:x b) (- (:x b) (:x a)))
      :y (+ (:y b) (- (:y b) (:y a)))}
     {:x (+ (:x a) (- (:x a) (:x b)))
      :y (+ (:y a) (- (:y a) (:y b)))}]))

(defn create-antinode [node v]
  {:x (+ (:x node) (- (:x node) (:x v)))
   :y (+ (:y node) (- (:y node) (:y v)))})

(defn- inside [{x :x y :y} input]
;   (println x y)
  (not (or (< x 0)
           (< y 0)
           (>= y (count input))
           (>= x (count (first input))))))

(defn create-resonant-antinode [ain m]
;   (println ain)
  (loop [stack [[(first ain) (second ain)]]
         result []]
    (let [[a b] (peek stack)]
;       (println a b result)
      (let [new-antinode (create-antinode a b)]
        (if-not (inside new-antinode m)
          result
          (recur
           (conj stack [new-antinode a]) (conj result new-antinode)))))))

(defn create-resonant-antinodes [ain m]
  (let [a (first ain)
        b (second ain)
        a-n (create-resonant-antinode [a b] m)
        b-n (create-resonant-antinode [b a] m)]
    (if (and a b)
      (concat [a b] a-n b-n))))

(defn find-antennas [input]
  (for [row (range (count input))
        col (range (count (first input)))
        :when (not= "." (matrix/at input {:x col :y row}))]
    {:x col :y row :value (matrix/at input {:x col :y row})}))

(defn part1 [input]
  (let [antennas  (find-antennas input)
        antenna-map (reduce (fn [acc antenna] (update-in acc [(:value antenna)] conj (select-keys antenna [:x :y])))
                            {}
                            antennas)]
    (->> antenna-map
         keys
         (map #(combo/combinations (get antenna-map %) 2))
         (map #(map (partial create-antinodes) %))
         flatten
         set
         (filter (fn [x]
                   (inside x input)))
         count)))

(defn part2 [input]
  (let [antennas  (find-antennas input)
        antenna-map (reduce (fn [acc antenna] (update-in acc [(:value antenna)] conj (select-keys antenna [:x :y])))
                            {}
                            antennas)]
    (->> antenna-map
         keys
         (map #(combo/combinations (get antenna-map %) 2))
         (map (fn [a] (map #(create-resonant-antinodes % input) a)))
         flatten
         set
         count)))

(defn solve [current-ns input-type]
  (let [input (->> (load-data current-ns input-type)
                   (mapv #(str/split % #"")))]
    {;      :part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

