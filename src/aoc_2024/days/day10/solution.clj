(ns aoc-2024.days.day10.solution
  (:require
   [aoc-2024.matrix :as matrix]
   [aoc-2024.utils :refer [load-data]]
   [clojure.string :as str]))

(defn find-starting-positions [m]
  (for [row (range (count m))
        col (range (count (first m)))
        :when (= 0 (matrix/at m {:x col :y row}))]
    {:x col :y row}))

(defn- walk [m start-pos]
  (loop [stack (list start-pos)
         visited #{start-pos}
         result 0]
    (if (empty? stack)
      result
      (let [cur (peek stack)
            cur-value (matrix/at m cur)
            new-stack (pop stack)
            new-visited (conj visited cur)]
        (if (= cur-value 9)
          (recur new-stack (conj new-visited cur) (inc result))
          (let [neighbours (->> (matrix/neighbours cur)
                                (filter #(let [v (matrix/at m %)]
                                           (and (some? v)
                                                (= 1 (- v cur-value))
                                                (not (new-visited %))))))]
            (recur (into new-stack neighbours) new-visited result)))))))

(defn part1 [input]
  (let [starts (find-starting-positions input)]
    (reduce + 0 (map (partial walk input) starts))))

(defn- walk2 [m start-pos]
  (loop [stack  [{:coord start-pos
                  :path (set [])}]
         result 0]
    (if (empty? stack)
      result
      (let [{:keys [coord path]} (peek stack)
            cur-value            (matrix/at m coord)
            new-stack            (pop stack)
            new-path             (conj path coord)]
        (if (= cur-value 9)
          (recur new-stack (inc result))
          (let [neighbours (->> (matrix/neighbours coord)
                                (filter #(let [v (matrix/at m %)]
                                           (and (some? v)
                                                (= 1 (- v cur-value))
                                                (not (new-path %)))))
                                (mapv (fn [c] {:coord c :path new-path})))]
            (recur (into new-stack neighbours) result)))))))

(defn part2 [input]
  (let [starts (find-starting-positions input)]
    (reduce + 0 (map (partial walk2 input) starts))))

(defn solve [current-ns input-type]
  (let [input (->> (load-data current-ns input-type)
                   (mapv (fn [row]
                           (->>
                            (str/split row #"")
                            (mapv parse-long)))))]

    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

