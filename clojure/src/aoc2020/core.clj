(ns aoc2020.core
  (:require [aoc2020.day-01]
            [aoc2020.day-02]))

(defn -main
  "Run the day using `lein run d01.p1`"
  [part]
  (case part
    "d01.p1" (println (aoc2020.day-01/part-1))
    "d01.p2" (println (aoc2020.day-01/part-2))
    (println "not found")))

