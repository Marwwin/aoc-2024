(ns aoc-2024.core)

;; Put all days that should get executed here
;; input should be :test or :input
(def days [{:day 1 :input :test}])

(defn- namespace-for-day [day]
  (str "aoc-2024.days.day" day ".solution"))

(defn print-result [{:keys [part1 part2]}]
  (println "  Part1:" part1)
  (println "  Part2:" part2))

(defn- run-day [{:keys [day input]}]
  (println "")
  (println (str "Running day" day " with " input " data"))
  (try
    (let [ns (namespace-for-day day)]
      (require (symbol ns))
      (let [solve-fn (resolve (symbol (str ns "/solve")))]
        (if solve-fn
          (-> (time (solve-fn ns input))
              print-result)
          (println (str "No solve function found for day" day)))))
    (catch Exception e
      (println (str "Error loading day" day " solution: " (.getMessage e))))))

(defn run-days []
  (println "Advent of Code 2024!!")
  (println "Running solutions!")
  (doseq [day days]
    (run-day day)))
(run-days)

(defn -main [& args]
  (run-days))
