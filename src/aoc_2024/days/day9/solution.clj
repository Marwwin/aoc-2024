(ns aoc-2024.days.day9.solution
  (:require
   [aoc-2024.utils :refer [load-data log]]
   [clojure.string :as str]))

(defn split-files-n-space [x]
  (->>
   x
   (map-indexed (fn [i c] [i c]))
   (group-by (fn [[i _]] (even? i)))
   (#(let [{a true b false} %]
       {:fs (map-indexed (fn [i [_ v]] (repeat v i)) a)
        :sp (map (fn [[_ v]] (repeat v :empty)) b)}))))

(defn walk [mem overlapping]
  (loop [mem mem
         overlapping overlapping
         result []]
    (if (empty? mem)
      result
      (let [x (first mem)]
        (if (= x :empty)
          (recur (rest mem) (butlast overlapping) (conj result (last overlapping)))
          (recur (rest mem) overlapping (conj result (first mem))))))))

(defn map-all [a b]
  (loop [xs a
         ys b
         result []]
    (if (and (empty? xs) (empty? ys))
      result
      (let [x (first xs)
            y (first ys)]
        (recur (rest xs) (rest ys) (cond-> result
                                     x (conj x)
                                     y (conj y)))))))

(defn part1 [{files :fs empties :sp }]
  (let [disk-map (flatten (map-all files empties))
        ovelapping (->> disk-map
                        (take-last (count (flatten empties)))
                        (filter #(not= % :empty)))
        mem (->> disk-map (take (count (flatten files))))
        organized-fs (walk mem ovelapping)
        checksum (map-indexed * organized-fs)]
    (reduce + checksum)))

(defn- swap-files [mem target end-i]
;   (println "swapping" target "at " end-i)
  (let [len (count target)]
    (loop [i 0
           prev []
           after mem]
      (if (> i end-i)
        mem
        (let [c (first after)]
;           (println i c (count c) (= (first c) :empty) (>= (count c) len))
          (if (and (= (first c) :empty)
                   (>= (count c) len))
            (assoc
             (vec
              (concat prev [target (repeat (- (count c) len) :empty)] (rest after)))
             (inc end-i)
             (repeat (count target) :empty))
            (recur (inc i) (conj prev c) (rest after))))))))

(defn clean-sequence [input-seq]
  (->> input-seq
       (remove empty?)
       (reduce (fn [acc curr]
                 (if (and (not-empty acc)
                          (every? #(= % :empty) curr)
                          (every? #(= % :empty) (last acc)))
                   (update acc (dec (count acc)) #(into % curr))
                   (conj acc curr)))
               [])))

;; TODO there might be an issue with index now after swap-files
(defn part2 [{files :fs empties :sp :as a}]
  (let [disk-map (map-all2 files empties)
        _ (println "created disk-map")
        fragmented-fs (loop [end-i (dec (count disk-map))
                             mem disk-map]
                        (if (= end-i 0)
                          mem
                          (let [curr (nth mem end-i)]
                            (if (= (first curr) :empty)
                              (recur (dec end-i) mem)
                              (let [new-mem (swap-files mem curr end-i)]
                                (recur
                                 (if (= (count new-mem) (count mem))
                                   (dec end-i)
                                   end-i)
                                 new-mem))))))
        _ (println "created fragmented-fs")
        cleaned-fs (clean-sequence fragmented-fs)
        _ (println "cleaned fragmented-fs")
        checksum (loop [fs (flatten cleaned-fs)
                        i 0
                        res 0]
                   (if (empty? fs)
                     res
                     (let [x (first fs)]
                       (if (= x :empty)
                         (recur (rest fs) (inc i) res)
                         (recur (rest fs) (inc i) (+ res (* i  x)))))))]
    (println "did checksum")
    checksum))

(defn solve [current-ns input-type]
  (let [input (-> (load-data current-ns input-type)
                  first
                  (str/split #"")
                  (->> (map parse-long))
                  split-files-n-space)]

    {;       :part1 (part1 input)
     :part2 (time (part2 input))}))

;; For repl
(solve (str *ns*) :input)

