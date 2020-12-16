(ns aoc2020.day-10
  (:require [clojure.string :refer [split-lines]]))

(defn get-input []
  (->> "./resources/day10.input"
    (slurp)
    (split-lines)
    (map #(Integer/parseInt %1))))

(defn get-diffs [xs]
  (->> xs
    (partition 2 1)
    (map #(Math/abs (apply - %1)))))

(defn add-boundaries [xs]
  (let [highest (apply max xs)]
      (conj xs 0 (+ 3 highest))))

(def combos
  (memoize
    (fn
      ([cur] 1)
      ([cur next & others]
       (let [voltage (+ cur next)]
         (if (> voltage 3)
           (apply combos next others)
           (let [head (apply combos next others)
                 tail (apply combos voltage others)]
             (+ head tail))))))))

; Answer should be 2760
(defn part-1 []
  (->> (get-input)
    (add-boundaries)
    (sort)
    (get-diffs)
    (frequencies)
    (vals)
    (apply *)))

; Answer should be 13816758796288
(defn part-2 []
  (->> (get-input)
    (add-boundaries)
    (sort)
    (get-diffs)
    (apply combos)))
