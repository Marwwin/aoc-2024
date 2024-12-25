(ns aoc-2024.days.day11.solution
  (:require
   [aoc-2024.utils :refer [load-data log]]
   [clojure.math :as math]
   [clojure.string :as str]))

;; part1 rules
; If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
;
; If the stone is engraved with a number that has an even number of digits, it is replaced by two stones.
; The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone.
; (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
;
; If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.

(defn amount-of-digits [n]
  (count (str n)))

(defn- split-stone-fn [stone]
   (let [len (count stone)]
     (-> stone
         (str/split #"")
         (->> (split-at (math/floor-div len 2))))))

(def split-stone (memoize split-stone-fn))

(defn apply-rules-fn [stone]
  (cond
    (= stone "0") ["1"]
    (even? (count stone)) (split-stone stone)
    :else  (str (* (parse-long stone) 2024))))

(def apply-rules (memoize apply-rules-fn))

(defn part1 [input]
  (loop [i 0
         result input]
    (if (= i 25)
      (count result)
      (recur (inc i) (flatten (map apply-rules result))))))

(defn part2 [input]
  (loop [i 0
         result ["0"]]
    (let [new-result (time (doall (flatten (map apply-rules result))))
          old (count result)
          new (count new-result)]
      (println i
               "- current size:" (count result)
               "- new size:" (count new-result)
               "- difference:" (- new old)
               "- zeros " (count (filter zero? new-result)))
      (if (= i 75)
        (count result)
        (recur (inc i) new-result)))))

(defn solve [current-ns input-type]
  (let [input (-> (load-data current-ns input-type)
                  first
                  (str/split #" ")
;                   (->> (map parse-long))
                  )]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

