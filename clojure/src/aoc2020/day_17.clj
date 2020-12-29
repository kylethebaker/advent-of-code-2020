(ns aoc2020.day-17
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;;------------------------------------------------
;; Shared
;;------------------------------------------------

(defn with-idx [xs]
  (map-indexed #(vector %1 %2) xs))

(defn initial-state [input]
  (for [[y row] (with-idx input)
        [x c] (with-idx row)]
    [[0 0 y x] (case c \# :active \. :inactive)]))

(defn get-input []
  (->> "./resources/day17.input"
    (slurp)
    (str/split-lines)
    (initial-state)
    (into {})))

(def ex
  (->> ".#.\n..#\n###"
    (str/split-lines)
    (initial-state)
    (into {})))

(defn w [[w _ _ _]] w)
(defn z [[_ z _ _]] z)
(defn y [[_ _ y _]] y)
(defn x [[_ _ _ x]] x)

;; Turns a list of [wzyx]'s into a [ws zs ys xs]
(defn transpose [wzyxs]
  (apply mapv hash-set wzyxs))

;; Returns the min/max values of the z/y/x coords
(defn bounds [wzyxs]
  (let [points (transpose wzyxs)
        [minw minz miny minx] (map #(dec (apply min %)) points)
        [maxw maxz maxy maxx] (map #(inc (apply max %)) points)]
    [[minw maxw] [minz maxz] [miny maxy] [minx maxx]]))

(defn active? [c] (= c :active))
(defn inactive? [c] (= c :inactive))

(defn get-cube [state wzyx]
  (get state wzyx :inactive))

;; Returns all of the points used in state, plus the outer shell
;; wall where new points could potentially flip
(defn points-with-shell [state]
  (let [points (keys state)
        [[minw maxw] [minz maxz] [miny maxy] [minx maxx]] (bounds points)]
    (for [w (range minw (inc maxw))
          z (range minz (inc maxz))
          y (range miny (inc maxy))
          x (range minx (inc maxx))]
    [w z y x])))

(defn neighbors [[w z y x]]
  (for [dw [-1 0 1]
        dz [-1 0 1]
        dy [-1 0 1]
        dx [-1 0 1]
        :when (not= [dw dz dy dx] [0 0 0 0])]
    (map + [w z y x] [dw dz dy dx])))

(defn count-active-neighbors [state wzyx]
  (->> wzyx
    (neighbors)
    (map #(get-cube state %))
    (filter active?)
    (count)))

(defn next-cube [state wzyx]
  (let [actives (count-active-neighbors state wzyx)
        cube (get-cube state wzyx)]
    (match [cube actives]
      [:active (:or 2 3)] :active
      [:inactive 3] :active
      :else :inactive)))

(defn next-board [state]
  (->> state
    (points-with-shell)
    (map #(vector %1 (next-cube state %1)))
    (into {})))

;;------------------------------------------------
;; Part 1 - Answer should be 276
;; Oops, I replaced everything with the part2
;; solution so this doesn't work anymore
;;------------------------------------------------

(defn part-1 []
  (->> (get-input)
    (iterate next-board)
    (take 7)
    (last)
    (vals)
    (filter active?)
    (count)))

;;------------------------------------------------
;; Part 2 - Answer should be 2136
;;------------------------------------------------

(defn part-2 []
  (->> (get-input)
    (iterate next-board)
    (take 7)
    (last)
    (vals)
    (filter active?)
    (count)))
