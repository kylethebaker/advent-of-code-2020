(ns aoc2020.day-10)

(defn get-input []
  (->> "./resources/day10.input"
    (slurp)
    (split-lines)
    (map #(Integer/parseInt %1))))

; Answer should be 2760
(defn part-1 []
  (->> (get-input)
    (cons 0)
    (sort)
    (reverse)
    (partition 2 1)
    (map #(apply - %1))
    (reduce #(update %1 %2 inc) {1 0, 3 1})
    (vals)
    (apply *)))

; Answer should be
(defn part-2 [] (sort (get-input)))
