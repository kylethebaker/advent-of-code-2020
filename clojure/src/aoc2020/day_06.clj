(ns aoc2020.day-06
  (:require [clojure.string :as str]
            [clojure.set :refer [union intersection]]))

(defn parse-group [group]
  (->> group
    (str/split-lines)
    (map seq)))

(defn get-input []
  (as-> "./resources/day06.input" _
    (slurp _)
    (str/split _ #"\n\n")
    (map parse-group _)))

(defn total-yes-count [group]
  (->> group
    (map set)
    (apply union)
    (count)))

(defn distinct-yes-count [group]
  (->> group
    (map set)
    (apply intersection)
    (count)))

; Answer should be 6878
(defn part-1 []
  (->> (get-input)
    (map total-yes-count)
    (apply +)))

; Answer should be 3464
(defn part-2 []
  (->> (get-input)
    (map distinct-yes-count)
    (apply +)))
