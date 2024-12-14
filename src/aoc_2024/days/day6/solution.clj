(ns aoc-2024.days.day6.solution
  (:require
   [aoc-2024.utils :refer [load-data log]]
   [clojure.string :as str]))

(defn find-all-elements-in-matrix [m e]
  (flatten
   (for [y (range (count m))
         x (range (count (first m)))]
     (if (= (get-in m [y x]) e)
       {:x x :y y}))))

(defn to-right [direction]
  (case direction
    :north :east
    :east  :south
    :south :west
    :west  :north))

(defn to-back [direction]
  (case direction
    :north :south
    :east  :west
    :south :north
    :west  :east))

(defn turn-right [coord]
  (update-in coord [:direction] to-right))

(defn turn-180
  [coord]
  (update-in coord [:direction] to-back))

(defn move-backwards [{:keys [x y direction] :as coord}]
  (case direction
    :north (update-in coord [:y] inc)
    :east (update-in coord [:x] dec)
    :south (update-in coord [:y] dec)
    :west (update-in coord [:x] inc)
    nil))

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
    (if (> i 3)
      (move coord)
      (let [new-n (move n)]
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

(defn find-obstacle [m guard]
  (loop [g guard
         i  0]
    (let [new-n (move g)]
      (case  (coord-at m new-n)
        nil nil
        "#" (do
;               (println g)
              g)
        (recur new-n (inc i))))))

(defn- walk2 [m guard]
  (loop [guard guard
         visited #{}
         obstacles #{}
         i 0]
;     (println guard)
    (case (coord-at m guard)
      nil obstacles
      "#" (let [gg (->> guard
                        move-backwards
                        turn-right
                        move)]
            (recur gg visited obstacles (inc i)))
      (let [obstacle (find-obstacle m (turn-right guard))
            directions (get visited (select-keys obstacle [:x :y]) nil)
            next-pos (move guard)
            next-visited? (get visited (select-keys next-pos [:x :y]))]
        (if (and directions (not next-visited?))
          (do
;               (println "found" next-pos)
;               (println "guard" guard)
;               (println "obs" obstacles)
;               (println "prev-obs" previous-coord)
;               (println "dir" directions)
;               (println visited)
;               (println "")
              (recur (move guard) (conj visited (select-keys guard [:x :y])) (conj obstacles (select-keys next-pos [:x :y])) (inc i)))
          (recur (move guard) (conj visited (select-keys guard [:x :y])) obstacles (inc i)))))))

(defn part1 [input]
  (let [guard (find-all input "^")
        guard (first guard)
        guard (assoc guard :direction :north)]
    (-> (walk input guard)
        log
        count)))

(defn part2 [input]
  (let [guard (-> input
                  (find-all "^")
                  first
                  (assoc :direction :north))]
    (println "guard starting at" guard)
    (-> (walk2 input guard)
        log
        count)))

(defn solve [current-ns input-type]
  (let [input (->>  (load-data current-ns input-type)
                    (map #(vec (str/split  % #"")))
                    vec)]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

