(ns aoc2020.day-05
  (:require [clojure.string :as str]))

(defn get-input []
  (->> "./resources/day05.input"
    (slurp)
    (str/split-lines)
    (map #(partition-all 7 %1))))

(defn get-half [seats side]
  (let [mid (/ (count seats) 2)]
    (let [[front back] (partition mid seats)]
      (case side
        (\F \L) front
        (\B \R) back))))

(defn get-seat [sides total-seats]
  (as-> total-seats _
    (range _)
    (reduce get-half _ sides)
    (first _)))

(defn get-seat-id [[rows, cols]]
  (let [row (get-seat rows 128)
        col (get-seat cols 8)]
    (+ (* row 8) col)))

(defn empty-with-neighbors? [seats seat]
  (and
    (not (contains? seats seat))
    (contains? seats (inc seat))
    (contains? seats (dec seat))))

(defn find-missing-seat [seats]
  (->> (range)
    (filter #(empty-with-neighbors? seats %1))
    (first)))

; Answer should be 994
(defn part-1 []
  (->> (get-input)
    (map get-seat-id)
    (apply max)))

; Answer should be 741
(defn part-2 []
  (->> (get-input)
    (map get-seat-id)
    (into #{})
    (find-missing-seat)))
