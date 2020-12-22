(ns aoc2020.day-15)

;;------------------------------------------------
;; Shared
;;------------------------------------------------

(defn get-input []
  (->> "./resources/day15.input"
    (slurp)
    (re-seq #"\d+")
    (map #(Integer/parseInt %))
    (vec)))

(defn add-turn [db turn n]
  (update-in db [n] conj turn))

(defn initialize [init-nums]
  (as-> init-nums _
    (reduce-kv #(add-turn %1 (inc %2) %3) {} _)
    [(count init-nums) (last init-nums) _]))

(defn only-seen-once? [db n]
  (->> n (get db) (count) (= 1)))

(defn set-turn-as-zero [[lt _ln db]]
  (let [turn (inc lt)]
    [turn 0 (add-turn db turn 0)]))

(defn set-turn-as-diff [[lt ln db]]
  (let [turn (inc lt)]
    (as-> db _
      (get _ ln)
      (take 2 _)
      (apply - _)
      [turn _ (add-turn db turn _)])))

(defn solve-next [[lt ln db]]
  (if (only-seen-once? db ln)
    (set-turn-as-zero [lt ln db])
    (set-turn-as-diff [lt ln db])))

(defn solve-nth [input n]
  (let [len (dec (count input))]
    (->> input
      (initialize)
      (iterate solve-next)
      (take (- n len))
      (last)
      (take 2))))

;;------------------------------------------------
;; Part 1
;;------------------------------------------------

;; Answer should be 257
(defn part-1 []
  (-> (get-input) (solve-nth 2020)))

;;------------------------------------------------
;; Part 2
;;------------------------------------------------

;; Answer should be 8546398
(defn part-2 []
  (-> (get-input) (solve-nth 300000000)))

