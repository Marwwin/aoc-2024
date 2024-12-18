(ns aoc-2024.matrix
  (:require
   [clojure.math :as math]))

(defn right [direction]
  (case direction
    :north :east
    :east  :south
    :south :west
    :west  :north))

(defn left [direction]
  (case direction
    :north :west
    :east  :north
    :south :east
    :west  :south))

(defn back [direction]
  (case direction
    :north :south
    :east  :west
    :south :north
    :west  :east))

(defn turn-right [coord]
  (update-in coord [:direction] right))

(defn turn-left [coord]
  (update-in coord [:direction] left))

(defn turn-180 [coord]
  (update-in coord [:direction] back))

(defn move
  ([{:keys [direction] :as coord}]
   (case direction
     :north (update-in coord [:y] dec)
     :east (update-in coord [:x] inc)
     :south (update-in coord [:y] inc)
     :west (update-in coord [:x] dec)))
  ([coord direction]
   (case direction
     :north (update coord :y dec)
     :east  (update coord :x inc)
     :south (update coord :y inc)
     :west  (update coord :x dec))))

(defn turn-n-move-left [coord]
  (-> coord
      turn-left
      move))

(defn turn-n-move-right [coord]
  (-> coord
      turn-right
      move))

(defn turn-n-move-180 [coord]
  (-> coord
      turn-180
      move))

(defn coords [c]
 (select-keys c [:x :y ] ))

(defn at [m {:keys [x y]}]
  (get-in m [y x] nil))

(defn coord-in-coll [coll {:keys [x y]}]
  (some #{{:x x :y y}} coll))

(defn find-all-elements-in-matrix [m e]
  (flatten
   (for [y (range (count m))
         x (range (count (first m)))]
     (if (= (get-in m [y x]) e)
       {:x x :y y}))))

(defn neighbours [{x :x y :y}]
  [{:x (dec x) :y y}
   {:x (inc x) :y y}
   {:x x :y (inc y)}
   {:x x :y (dec y)}])

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


