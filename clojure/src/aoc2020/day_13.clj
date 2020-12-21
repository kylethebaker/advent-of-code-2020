(ns aoc2020.day-13
  (:require [clojure.core.match :refer [match]]
            [clojure.string :refer [split-lines]]))

;;------------------------------------------------
;; Shared
;;------------------------------------------------

(defn get-input []
  (->> "./resources/day13.input"
    (slurp)
    (split-lines)))

;;------------------------------------------------
;; Part 1
;;------------------------------------------------

(defn parse-with-min-time [[earliest schedule]]
  (let [busses (re-seq #"\d+" schedule)]
    [(Integer/parseInt earliest)
     (map #(Integer/parseInt %) busses)]))

(defn earliest-departure [busses min-time]
  (loop [n min-time]
    (let [found (filter #(zero? (mod n %)) busses)]
      (if (empty? found)
        (recur (inc n))
        [n (first found)]))))

;; Answer should be 2298
(defn part-1 []
  (let [[min-time busses] (parse-with-min-time (get-input))
        [early-t early-bus] (earliest-departure busses min-time)]
    (* early-bus (- early-t min-time))))

;;------------------------------------------------
;; Part 2
;;------------------------------------------------

(defn parse-with-offsets [[_time schedule]]
  (->> schedule
    (re-seq #"(?:\d+|x)")
    (map-indexed #(vector %1 %2))
    (remove (fn [[_ b]] (= b "x")))
    (map (fn [[t b]] [t (Integer/parseInt b)]))))

(defn check-if-valid [timestamp busses]
  (loop [[[offset bus] & others] busses
         multiple 1]
    (let [t (+ timestamp offset)]
      (if-not (zero? (mod t bus))
          [:new-interval multiple]
        (if-not (nil? others)
          (recur others (* bus multiple))
          [:found timestamp])))))

;; Answer should be 783685719679632
(defn part-2 []
  (let [busses (parse-with-offsets (get-input))
        [_ interval] (first busses)]
    (loop [t interval]
      (match (check-if-valid t busses)
        [:new-interval i] (recur (+ t i))
        [:found earliest] earliest))))
