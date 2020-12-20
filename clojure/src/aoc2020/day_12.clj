(ns aoc2020.day-12
  (:require [clojure.core.match :refer [match]]))

;;------------------------------------------------
;; Shared
;;------------------------------------------------

(defn parse-instruction [[_ dir dist]]
  [dir (Integer/parseInt dist)])

(defn get-input []
  (->> "./resources/day12.input"
    (slurp)
    (re-seq #"(\w){1}(\d+)\n")
    (map parse-instruction)))

(defn move [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn move-lateral [xy dir dist]
  (case dir
    "N" (move xy [0 dist])
    "E" (move xy [dist 0])
    "S" (move xy [0 (- dist)])
    "W" (move xy [(- dist) 0])))

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

;;------------------------------------------------
;; Part 1
;;------------------------------------------------

(defn turn-ship [facing dir deg]
  (as-> ["N" "E" "S" "W"] _
    (if (= dir "L") (reverse _) _)
    (cycle _)
    (drop-while #(not= facing %1) _)
    (nth _ (/ deg 90))))

(defn travel-ship-step [[facing point] [dir dist]]
  (match dir
    "F" [facing (move-lateral point facing dist)]
    (:or "L" "R") [(turn-ship facing dir dist) point]
    :else [facing (move-lateral point dir dist)]))

;; Answer should be 2057
(defn part-1 []
  (->> (get-input)
    (reduce travel-ship-step ["E" [0 0]])
    (last)
    (manhattan [0 0])))

;;------------------------------------------------
;; Part 2
;;------------------------------------------------

(defn cw-90 [[x y]] [y, (- x)])
(defn ccw-90 [[x y]] [(- y), x])

(defn rotate-waypoint [wp dir deg]
  (let [r-90 (case dir "L" ccw-90 "R" cw-90)
        n (/ deg 90)
        rotate (apply comp (repeat n r-90))]
    (rotate wp)))

(defn move-to-waypoint [ship wp n]
  (let [move-fn (apply comp (repeat n #(move % wp)))]
    (move-fn ship)))

(defn travel-with-waypoint [[ship wp] [dir dist]]
  (match dir
    "F" [(move-to-waypoint ship wp dist) wp]
    (:or "L" "R") [ship (rotate-waypoint wp dir dist)]
    :else [ship (move-lateral wp dir dist)]))

;; Answer should be 71504
(defn part-2 []
  (->> (get-input)
    (reduce travel-with-waypoint [[0 0] [10 1]])
    (first)
    (manhattan [0 0])))
