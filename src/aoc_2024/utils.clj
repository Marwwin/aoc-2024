(ns aoc-2024.utils
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn namespace-to-path [current-ns input]
  (-> current-ns
      (str/split #".solution")
      first
      (str/replace #"-" "_")
      (str/split #"\.")
      (conj (name input))
      (->> (str/join "/"))))

(defn load-data [current-ns input]
  (let [path (namespace-to-path current-ns input)]
    (->> (io/resource path)
         slurp
         (str/split-lines)
         (map str/trim))))

(defn log [x]
  (println x)
  x)

(defn zip [& colls]
  (apply map vector colls))

(defn manhattan [a b]
  (let [{ax :x ay :y} a
        {bx :x by :y} b]
    (+ (abs (- by ay))
       (abs (- bx ax)))))

(manhattan {:x 1 :y 5} {:x 4 :y 2})

(defn remove-first [xs v]
  (loop [xs xs
         res '()
         found false]
    (if-not (seq xs)
      res
      (let [is-v (and (false? found) (= (first xs) v))
            new-res (if is-v res (conj res (first xs)))
            new-found (if is-v true found)]
        (recur (rest xs) new-res new-found)))))


