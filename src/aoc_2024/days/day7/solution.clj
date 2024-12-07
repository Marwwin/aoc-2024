(ns aoc-2024.days.day7.solution
  (:require
   [aoc-2024.utils :refer [load-data zip]]
   [clojure.math.combinatorics :as comb]
   [clojure.pprint :as pprint]
   [clojure.string :as str]))

(defn get-combinations [amount]
  (mapv (fn [n] (concat (repeat n +) (repeat (-  amount n) *))) (range (inc amount))))

(defn is-c [target ns combs]
;   (pprint/pprint combs)
  (let [a (->> combs
               (map (fn [fns]
    ;                  (println "fns")
    ;                  (pprint/pprint  fns)
    ;                  (println "")
                      (= target  (reduce (fn [acc,[f n]]
;                                            (println "fs" f)
                                           (f acc n))
                                         (first ns)
                                         (zip fns (rest ns)))))))]
;     (println target a)
    (if (some true? a)
      target
      0)))

(defn part1 [input]
  (reduce (fn [acc {:keys [target ns]}]
            (let [cs (get-combinations (dec (count ns)))
;                   _ (println "")
;                   _ (pprint/pprint cs)
                  xs  (partition (dec (count ns)) (flatten (mapv #(comb/permutations %) cs)))]
;               (pprint/pprint xs)
              (+ acc (is-c target ns xs)))) 0 input))

(defn part2 [input])

(defn parse [line]
  (let [[target, xs] (str/split line #":")]
    {:target (parse-long target)
     :ns     (mapv parse-long (str/split (str/trim xs) #" "))}))

(defn solve [current-ns input-type]
  (let [input (->> (load-data current-ns input-type)
                   (mapv parse))]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

