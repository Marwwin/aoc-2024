(ns aoc-2024.days.day6.solution
  (:require
   [aoc-2024.utils :refer [load-data]]
   [clojure.string :as str]))

(defn find-all [m e]
  (flatten
   (for [y (range (count m))
         x (range (count (first m)))]
     (if (= (get-in m [y x]) e)
       {:x x :y y}
       []))))

(defn turn-right [coord]
  (update-in coord [:direction] (fn [direction]
                                  (case direction
                                    :north :east
                                    :east  :south
                                    :south :west
                                    :west  :north))))

(defn move [{:keys [x y direction] :as coord}]
  (case direction
    :north (update-in coord [:y] dec)
    :east (update-in coord [:x] inc)
    :south (update-in coord [:y] inc)
  :west (update-in coord [:x] dec)))

(defn coord-at [m {:keys [x y]}]
  (get-in m [y x] nil))

(defn turn-and-move [m {:keys [x y direction] :as coord}]
  (loop [n coord
         i 0]
;     (println "new loop" n)
    (if (> i 3)
      (move coord)
    (let [new-n (move n)]
;       (println new-n (coord-at m new-n))
      (if-not (= (coord-at m new-n) "#")
        new-n
        (recur (turn-right n) (inc i)))))))

(defn walk [m guard]
  (loop [guard guard
           visited (set [])
           i 0]
        (let [next-g (turn-and-move m guard)]
          (if-not (coord-at m guard)
            visited
            (recur next-g (conj visited (select-keys guard [:x :y])) (inc i))))))


(defn part1 [input]
;   (clojure.pprint/pprint input)
  (let [guard (find-all input "^")
        guard (first guard)
        guard (assoc guard :direction :north)]
    (println "guard starting at" guard)
    (println(get input 1))
    (-> (walk input guard)
        count
        )
    ))

(defn part2 [input])

(defn solve [current-ns input-type]
  (let [input (->>  (load-data current-ns input-type)
                    (map #(vec (str/split  % #"")))
                    vec)]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

