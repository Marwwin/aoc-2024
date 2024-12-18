(ns aoc-2024.days.day12.solution
  (:require
   [aoc-2024.matrix :as matrix]
   [aoc-2024.utils :refer [load-data log]]
   [clojure.math :as math]
   [clojure.string :as str]))

(defn neighbours [{x :x y :y}]
  [{:x (dec x) :y y}
   {:x (inc x) :y y}
   {:x x :y (inc y)}
   {:x x :y (dec y)}])

(defn at [m {x :x y :y}]
  (get-in m [y x] nil))

(defn inside [m {x :x y :y}]
  (cond
    (< x 0) false
    (< y 0) false
    (>= x (count (first m))) false
    (>= y (count m)) false
    :else true))

(defn index-to-coord [m i]
  {:x (math/floor-mod i (count (first m)))
   :y (math/floor-div i (count m))})

(defn bfs [m start target]
  (loop [stack (vector {:x (:x start) :y (:y start)})
         visited #{{:x (:x start) :y (:y start)}}
         perimeter []
         i 0]
    (if (empty? stack)
      {:visited visited :perimeter perimeter}
      (let [pos           (peek stack)
            neigs         (->> pos
                               neighbours
                               (filter #(not (contains? visited %))))
            pos-perimeter (filter #(not= (at m %) target) neigs)
            pos-seen      (filter #(= (at m %) target) neigs)
            new-stack     (pop stack)]
        (recur (into new-stack (filter #(inside m %) (remove (set new-stack) pos-seen))) (conj visited pos) (into perimeter pos-perimeter) (inc i))))))

(defn part1 [input]
  (let [m (mapv #(str/split % #"") input)]
    (loop [i       0
           all-visited #{}
           result  0]
      (let [coord  (index-to-coord m i)
            target (at m coord)]
        (if (nil? target)
          result
          (if (contains? all-visited coord)
            (recur (inc i) all-visited result)
            (let [{:keys [visited perimeter]} (bfs m coord target)]
              (recur (inc i) (into all-visited visited) (+ result (* (count visited) (count perimeter)))))))))))

(defn get-top-right-in-perimeter [list-of-coords]
  (let [min-x (apply min (map (fn [{x :x y :y}] x) list-of-coords))
        xs (filter #(= min-x (:x %)) list-of-coords)
        min-y (apply min (map (fn [{x :x y :y}] y) xs))]
    {:x min-x :y min-y}))

(defn walk-perimeter [perimeter area]
  (println perimeter)
  (let [start (assoc (get-top-right-in-perimeter perimeter) :direction :east) ]
    (loop [stack (list start)
           visited #{start}
           ps perimeter
           result 0]
      (if (empty? stack)
        (do (println ps) result)
        (let [curr (peek stack)]
          (let [new-stack (pop stack)
                [left forward right forward-left forward-right] [
                    (matrix/coord-in-coll ps (matrix/turn-n-move-left curr))
                    (matrix/coord-in-coll ps (matrix/move curr))
                    (matrix/coord-in-coll ps (matrix/turn-n-move-right curr))
                    (matrix/coord-in-coll ps (-> curr matrix/move matrix/turn-n-move-left))
                    (matrix/coord-in-coll ps (-> curr matrix/move matrix/turn-n-move-right))
                                                                 ]
                _ (println left forward right forward-left forward-right)
               {:keys [next-coord res]} (cond
                             (->> curr matrix/turn-n-move-right matrix/coords (contains? area) not)  {:next-coord (matrix/turn-n-move-right curr):res 1}
                             (->> curr matrix/move matrix/coords (contains? area) not)  {:next-coord (matrix/move curr) :res 0}
                             (->> curr matrix/turn-n-move-left matrix/coords (contains? area) not)  {:next-coord (matrix/turn-n-move-left curr) :res 1}
                             :else {:next-coord (matrix/turn-180 curr) :res 2})
               new-ps (remove #{(matrix/coords curr)} ps)]
            (println "removing" #{(matrix/coords curr)} )
            (println "removed " new-ps)
            (println "")
;              (println next-coord)
            (recur (if (contains? visited next-coord) new-stack (conj new-stack next-coord))
                   (conj visited curr)
                   new-ps
                   (+ result res))))))))


(defn walk-perimeter2 [perimeter area]
  (println perimeter)
  (let [start (assoc (first perimeter) :direction :east)
        start (matrix/move start :north)]
   ; (println "start walk perim" start)
    (loop [stack (list start)
           visited #{start}
           result 0]
    ;  (println "stack" "stack" stack (empty? stack))
      (if (empty? stack)
        (do result)
        (let [curr (peek stack)]
          (let [new-stack (pop stack)
               {:keys [next-coord res]} (cond
                             (->> curr matrix/turn-n-move-right matrix/coords (contains? area) not)  {:next-coord (matrix/turn-n-move-right curr):res 1}
                             (->> curr matrix/move matrix/coords (contains? area) not)  {:next-coord (matrix/move curr) :res 0}
                             (->> curr matrix/turn-n-move-left matrix/coords (contains? area) not)  {:next-coord (matrix/turn-n-move-left curr) :res 1}
                             :else {:next-coord (matrix/turn-180 curr) :res 2}) ]
;              (println next-coord)
            (recur (if (contains? visited next-coord) new-stack (conj new-stack next-coord))
                   (conj visited curr)
                   (+ result res))))))))

(defn part2 [input]
  (let [m (mapv #(str/split % #"") input)]
    (loop [i       0
           all-visited #{}
           result  0]
      (let [coord  (index-to-coord m i)
            target (at m coord)]
        (if (nil? target)
          result
          (if (contains? all-visited coord)
            (recur (inc i) all-visited result)
            (let [{:keys [visited perimeter]} (bfs m coord target)
                  _ (println "perim for target" target)
                  perimeter-len (walk-perimeter perimeter visited)]
               (println "done" target  (count visited) perimeter-len)
              (recur (inc i) (into all-visited visited) (+ result (* (count visited) perimeter-len))))))))))

(defn solve [current-ns input-type]
  (let [input (load-data current-ns input-type)]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; for repl
(solve (str *ns*) :test)

