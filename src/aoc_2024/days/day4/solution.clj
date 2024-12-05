(ns aoc-2024.days.day4.solution
  (:require
   [aoc-2024.utils :refer [load-data]]
   [clojure.string :as str]))

(defn coords-in-all-directions [{x :x y :y}]
  (apply mapv vector
         (for [h [0 1 2 3]
               v [0]]
           [{:x (+ x h) :y v}
            {:x (- x h) :y v}
            {:x x       :y (+ y h)}
            {:x x       :y (- y h)}
            {:x (- x h) :y (- y h)}
            {:x (+ x h) :y (- y h)}
            {:x (+ x h) :y (+ y h)}
            {:x (- x h) :y (+ y h)}])))

(defn coord-at [m {x :x y :y}]
  (get-in m [y x] nil))

(defn part1 [input]
  (->> (for [row (range (count input))
             col (range (count (first input)))]
         (->> (coords-in-all-directions {:x col :y row})
              (map (fn [n] (->> n
                                (map (partial coord-at input))
                                str/join)))))
       flatten
       (filter
        #(= % "XMAS"))
       count))

(defn is-x-mas [m {x :x y :y}]
  (let [correct [\M \S]
        a       (sort [(coord-at m {:x (dec x) :y (dec y)})
                       (coord-at m {:x (inc x) :y (inc y)})])
        b       (sort [(coord-at m {:x (dec x) :y (inc y)})
                       (coord-at m {:x (inc x) :y (dec y)})])]
    (and (= correct a)
         (= correct b))))

(defn part2 [input]
  (->> (for [row (range (count input))
             col (range (count (first input)))]
         (if (= (coord-at input {:x col :y row}) \A)
           (is-x-mas input {:x col :y row})
           nil))
       (filter true?)
       count))

(defn solve [current-ns input-type]
  (let [input (->> (load-data current-ns input-type)
                   vec)]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

