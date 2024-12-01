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


