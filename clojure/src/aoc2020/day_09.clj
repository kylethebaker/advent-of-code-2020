(ns aoc2020.day-09
  (:require [clojure.string :refer [split-lines]]
            [clojure.math.combinatorics :refer [combinations]]))

(defn get-input []
  (->> "./resources/day09.input"
    (slurp)
    (split-lines)
    (map bigint)))

(defn sums-of-every-pair [preamble]
  (as-> preamble _
    (combinations _ 2)
    (map #(apply + %) _)
    (into #{} _)))

(defn find-first-invalid [cipher]
  (loop [cipher cipher]
    (let [[preamble [check & others]] (split-at 25 cipher)]
      (let [sums (sums-of-every-pair preamble)]
        (cond
          (not (contains? sums check)) check
          (nil? others) :you-failed
          :else (recur (next cipher)))))))

(defn get-range [start offset cipher]
  (as-> cipher _
    (nthrest _ start)
    (take offset _)))

(defn find-contiguous-sum [cipher target]
  (loop [start 0 offset 0 sum 0]
    (let [new-sum (->> start (+ offset) (get cipher) (+ sum))]
      (cond
        (> (+ start offset) (count cipher)) :you-failed
        (= new-sum target) (get-range start offset cipher)
        (< new-sum target) (recur start (inc offset) new-sum)
        (> new-sum target) (recur (inc start) 0 0)))))

; Answer should be 1639024365
(defn part-1 []
  (find-first-invalid (get-input)))

; Answer should be 219202240
(defn part-2 []
  (let [input (get-input)]
    (as-> input _
      (find-first-invalid _)
      (find-contiguous-sum (vec input) _)
      (+ (apply min _) (apply max _)))))
