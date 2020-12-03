(ns aoc2020.day-03
  (:require [clojure.math.combinatorics :as cmb]
            [clojure.string :as str]))

(defn get-grid []
  (->> "./resources/day03.input"
    (slurp)
    (str/split-lines)
    (map char-array)
    (vec)))

(defn get-in-grid [grid [y x]]
  (if-let [row (get grid y)]
    (nth (cycle row) x)))

(defn create-slope-fn [[down right]]
  (fn [[y x]] [(+ y down) (+ x right)]))

(defn count-trees-on-slope [grid, slope]
  (let [get-next (create-slope-fn slope)]
    (loop [point [0 0] trees 0]
      (let [next-point (get-next point)]
        (case (get-in-grid grid point)
          \# (recur next-point (inc trees))
          \. (recur next-point trees)
          nil trees)))))

; Answer should be 252
(defn part-1 []
  (count-trees-on-slope (get-grid) [1 3]))

; Answer should be 2608962048
(defn part-2 []
  (let [grid (get-grid)]
    (->> [[1 1] [1 3] [1 5] [1 7] [2 1]]
       (map (partial count-trees-on-slope grid))
       (apply *))))
