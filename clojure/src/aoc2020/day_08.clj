(ns aoc2020.day-08)

(defn get-input []
  (->> "./resources/day08.input"
    (slurp)
    (re-seq #"(acc|jmp|nop) ((:?\+|-)\d+)\n")
    (map (fn [[_ ins ctr]] [ins (Integer/parseInt ctr)]))
    (vec)))

(defn do-step [[acc pc] [ins v]]
  (case ins
    "acc" [(+ acc v) (inc pc)]
    "jmp" [acc (+ pc v)]
    "nop" [acc (inc pc)]))

(defn run-until-loop-or-exit [prog]
  (loop [visited #{0} pc 0 acc 0]
    (let [[next-acc next-pc] (do-step [acc pc] (prog pc))]
      (if (= next-pc (count prog))
        [:exit next-acc]
        (if-not (contains? visited next-pc)
          (recur (conj visited next-pc) next-pc next-acc)
          [:loop acc])))))

(defn swap-instruction [prog pc]
  (let [[ins v] (get prog pc)]
    (case ins
      "jmp" (assoc prog pc ["nop" v])
      "nop" (assoc prog pc ["jmp" v])
      nil)))

(defn swap-until-it-runs [prog]
  (loop [pc 0]
    (let [new-prog (swap-instruction prog pc)]
      (if-not new-prog
        (recur (inc pc))
        (let [[result acc] (run-until-loop-or-exit new-prog)]
          (case result
            :exit [result acc]
            :loop (recur (inc pc))))))))

; Answer should be 1859
(defn part-1 []
  (run-until-loop-or-exit (get-input)))

; Answer should be 1235
(defn part-2 []
  (swap-until-it-runs (get-input)))
