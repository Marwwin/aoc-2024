(ns aoc-2024.days.day7.solution
  (:require
   [aoc-2024.utils :refer [load-data]]
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]))

(defn get-combinations [amount]
  (let [types (concat [*] [+])]
    (for [combination (apply comb/cartesian-product (repeat amount types))]
      (vec combination))))

(defn reduce-comb [fns nums target]
  (loop [result (first nums)
         fns  fns
         nums (rest nums)]
    (let [f (first fns)]
      (cond
        (= result target) target
        (> result target) false
        (nil? f)          false
        :else (recur (f result (first nums)) (rest fns) (rest nums))))))

(defn is-c [target nums combs]
  (loop [cs combs]
    (if (nil? (first cs))
      0
      (if (reduce-comb (first cs) nums target)
        target
        (recur (rest cs))))))

(defn part1 [input]
  (reduce (fn [acc {:keys [target nums]}]
            (let [cs (get-combinations (dec (count nums)))]
              (+ acc (is-c target nums cs)))) 0 input))

(defn dfs [{:keys [target nums]}]
  (loop [stack (list {:result (first nums)
                      :index 0})]
    (if (empty? stack)
      false
      (let [t (peek stack)]
        (cond
          (and (= (:result t) target)
               (= (:index t) (dec (count nums)))) true
          (= (:index t) (dec (count nums)))       (recur (pop stack))
          :else                                   (let [next-n (-> nums (nth (inc (:index t))))]
                                                    (recur (into (pop stack)
                                                                 [{:result (+ (:result t) next-n)
                                                                   :index (inc (:index t))}
                                                                  {:result (* (:result t) next-n)
                                                                   :index (inc (:index t))}
                                                                  {:result (parse-long (str (:result t) next-n))
                                                                   :index (inc (:index t))}]))))))))

(defn part2 [input]
  (let [a (map (fn [row]
                 (if (dfs row) (:target row) 0)) input)]
    (apply + a)))

(defn parse [line]
  (let [[target, xs] (str/split line #":")]
    {:target (parse-long target)
     :nums   (map parse-long (str/split (str/trim xs) #" "))}))

(defn solve [current-ns input-type]
  (let [input (->> (load-data current-ns input-type)
                   (mapv parse))]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl

(solve (str *ns*) :input)
