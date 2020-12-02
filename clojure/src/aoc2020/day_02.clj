(ns aoc2020.day-02
  (:require [clojure.math.combinatorics :as cmb]))

(defn parse-password [[_ p1 p2 l pw]]
  [(Integer/parseInt p1), (Integer/parseInt p2), l, pw])

(defn get-passwords []
  (->> "./resources/day02.input"
    (slurp)
    (re-seq #"(\d+)-(\d+) ([a-z]): ([a-z]+)\n")
    (map parse-password)))

(defn count-letter [letter str]
  (get (frequencies str) (.charAt letter 0) 0))

(defn is-valid-simple-pw [[min max letter pw]]
  (let [count (count-letter letter pw)]
    (<= min count max)))

(defn is-letter-at [pw letter pos]
  (= letter (str (get pw (dec pos)))))

(defn is-valid-complex-pw [[p1 p2 letter pw]]
  (case (map #(is-letter-at pw letter %) [p1 p2])
    [true, true] false
    [false, false] false
    [true, false] true
    [false, true] true))

; Correct answer should be 447
(defn part-1 []
  (->> (get-passwords)
    (filter is-valid-simple-pw)
    (count)))

; Correct answer should be 249
(defn part-2 []
  (->> (get-passwords)
    (filter is-valid-complex-pw)
    (count)))
