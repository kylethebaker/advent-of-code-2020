(ns aoc2020.day-11
  (:require [clojure.string :refer [split-lines]]))

(defn seat-type [c]
  (case c
    \. :floor
    \# :occupied
    \L :empty))

(defn input-kv [seats]
  (for [y (-> seats count range)
        x (-> seats (get 0) count range)
        :let [point [y x]
              seat (get-in seats [y x])]]
    [point (seat-type seat)]))

(defn get-input []
  (->> "./resources/day11.input"
    (slurp)
    (split-lines)
    (map vec)
    (vec)
    (input-kv)
    (into {})))

(defn floor? [s] (= s :floor))
(defn unoccupied? [s] (= s :empty))
(defn occupied? [s] (= s :occupied))

(defn adjacents [[y x]]
  (for [dy [-1 0 1]
        dx [-1 0 1]
        :when (not= [dy dx] [0 0])]
    [(+ y dy) (+ x dx)]))

(defn surrounding [seats point]
  (->> point
    (adjacents)
    (map #(seats %))
    (filter occupied?)
    (count)))

(defn line-of-site [point [dy dx]]
  (iterate (fn [[y x]] [(+ y dy) (+ x dx)]) point))

(defn in-line-of-site [seats point]
    (for [dy [-1 0 1]
          dx [-1 0 1]
          :when (not= [dy dx] [0 0])
          :let [los (line-of-site point [dy dx])]]
      (->> los
        (drop 1)
        (map #(seats %))
        (drop-while floor?)
        (first))))

(defn visible [seats point]
  (->> point
    (in-line-of-site seats)
    (filter occupied?)
    (count)))

(defn next-seat-fn [count-fn needed]
  (fn [seats point]
    (let [seat (seats point)]
      (if (floor? seat)
        seat
        (let [cnt (count-fn seats point)]
          (cond
            (and (unoccupied? seat) (zero? cnt)) :occupied
            (and (occupied? seat) (>= cnt needed)) :empty
            :else seat))))))

(defn do-round [next-seat seats]
  (->> seats
    (keys)
    (map (fn [p] [p (next-seat seats p)]))
    (into {})))

(defn do-all-rounds [seats count-fn needed]
  (let [next-seat (next-seat-fn count-fn needed)]
    (loop [prev seats]
      (let [current (do-round next-seat prev)]
        (if (not= current prev)
          (recur current)
          current)))))

(defn count-occupied [seats]
  (->> seats
    (vals)
    (filter occupied?)
    (count)))

; Answer should be 2418
(defn part-1 []
  (-> (get-input)
    (do-all-rounds surrounding 4)
    (count-occupied)))

; Answer should be 2144
(defn part-2 []
  (-> (get-input)
    (do-all-rounds visible 5)
    (count-occupied)))
