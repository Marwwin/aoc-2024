(ns aoc-2024.days.day5.solution
  (:require
   [aoc-2024.utils :refer [load-data log]]
   [clojure.math :as math]
   [clojure.string :as str]
   [clojure.test :refer [is]]))

(defn get-middle [v]
  (if-not (seq v)
    0
    (nth v (int (math/floor (/  (count v) 2))))))

(defn ordered? [rules upd]
  (loop [i 0
         xs upd]
    (let [rule (get rules (first xs))]
      (cond
        (not (seq xs)) {:ok true :i i}
        (not (every? (set rule) (rest xs))) {:ok false :i i}
        :else (recur (inc i) (rest xs))))))

(defn- swap-i [upd x y]
  {:pre [(is (< x y))]}
  (loop [u upd
         i 0
         res (list)]
    (if-not (seq u)
      (reverse res)
      (let [n (cond
                (= i x) (nth upd y)
                (= i y) (nth upd x)
                :else (first u))]
        (recur (rest u) (inc i) (conj res n))))))

(defn put-last [u i]
  (concat (take  i u)
          (take-last (- (count u)  (inc i)) u)
          [(nth u i)]))

(defn re-order [rules upd]
  (loop [u  upd
         res []]
    (if (= (count res) (count upd))
      res
      (let [ord (ordered? rules u)]
        (if (:ok ord)
          (recur (rest u) (conj res (first u)) )
          (recur (concat res (put-last u (:i ord))) [] ))))))

(defn part1 [{:keys [rules updates]}]
  (->> (:corrects updates)
       (map get-middle)
       (reduce + 0)))

(defn part2 [{:keys [rules updates]}]
  (->> (:incorrects updates)
       (map (partial re-order rules))
       (map get-middle) (reduce + 0)))

(defn parse-updates [rules updates]
  (->> updates
       (reduce
        (fn [acc upd]
          (loop [xs upd]
            (if-not (seq (rest xs))
              (update acc :corrects conj upd)
              (if-not (:ok (ordered? rules upd))
                (update acc :incorrects conj upd)
                (recur (rest xs))))))
        {:corrects [] :incorrects []})))

(defn parse [input]
  (loop [lines input
         rules? true
         ordering-rules {}
         updates []]
    (let [line (first lines)]
      (if-not line
        {:rules ordering-rules :updates (parse-updates ordering-rules updates)}
        (let [[a b] (when rules? (-> line
                                     (str/split #"\|")
                                     (->> (mapv parse-long))))
              ordering-rules (if (and a b) (update ordering-rules a conj b) ordering-rules)
              upd (when-not rules? (-> line
                                       (str/split #",")
                                       (->> (mapv parse-long))))
              updates (if upd (conj updates upd) updates)]
          (if (= line "")
            (recur (rest lines) false ordering-rules updates)
            (recur (rest lines) rules? ordering-rules updates)))))))

(defn solve [current-ns input-type]
  (let [input (-> (load-data current-ns input-type)
                  parse)]
    {:part1 (part1 input)
     :part2 (part2 input)}))

;; For repl
(solve (str *ns*) :input)

