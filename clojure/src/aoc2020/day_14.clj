(ns aoc2020.day-14
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;;------------------------------------------------
;; Shared
;;------------------------------------------------

(defn parse-mask [mask-str]
  (->> mask-str
    (re-find #"[10X]+$")
    (reverse)
    (vec)))

(defn parse-mem [mem-str]
  (->> mem-str
    (re-find #"mem\[(\d+)\] = (\d+)")
    (rest)
    (map #(Integer/parseInt %))
    (vec)))

(defn parse-line [line]
  (if (str/starts-with? line "mask")
    [:mask (parse-mask line)]
    [:mem (parse-mem line)]))

(defn to-program [prog [type ins]]
  (case type
    :mask (conj prog {:mask ins :mems []})
    :mem (update-in prog [(dec (count prog)) :mems] conj ins)))

(defn get-input []
  (->> "./resources/day14.input"
    (slurp)
    (str/split-lines)
    (map parse-line)
    (reduce to-program [])))

(defn make-mask-fn [mask map-fn]
  (->> mask
    (map-indexed map-fn)
    (remove nil?)
    (apply comp)))

;;------------------------------------------------
;; Part 1
;;------------------------------------------------

(defn val-bit-fn [pos val]
  (case val
    \1 #(bit-set % pos)
    \0 #(bit-clear % pos)
    \X nil))

(defn solve-segment [{:keys [mask mems]}]
  (let [mask-fn (make-mask-fn mask val-bit-fn)]
    (->> mems
      (map (fn [[reg v]] [reg (mask-fn v)]))
      (into {}))))

;; Answer should be 14954914379452
(defn part-1 []
  (->> (get-input)
  (map solve-segment)
  (into {})
  (vals)
  (apply +)))

;;------------------------------------------------
;; Part 2
;;------------------------------------------------

(defn mem-bit-fn [pos val]
  (case val
    \1 #(bit-set % pos)
    nil))

(defn float-bit-fn [[pos val]]
  (case val
    1 #(bit-set % pos)
    0 #(bit-clear % pos)
    nil))

(defn get-float-bits [mask]
  (->> mask
    (map-indexed #(case %2 \X [[%1 0] [%1 1]] nil))
    (remove nil?)
    (apply combo/cartesian-product)))

(defn expand-floaters [floaters addr]
  (as-> floaters _
    (map #(map float-bit-fn %) _)
    (map #(apply comp %) _)
    (map #(% addr) _)))

(defn solve-mem [mask [addr v]]
  (let [mask-fn (make-mask-fn mask mem-bit-fn)
        floaters (get-float-bits mask)]
    (->> addr
      (mask-fn)
      (expand-floaters floaters)
      (map #(vector % v))
      (into {}))))

(defn solve-segment2 [{:keys [mask mems]}]
  (->> mems
    (map #(solve-mem mask %))
    (into {})))

;; Answer should be 3415488160714
(defn part-2 []
  (->> (get-input)
    (map solve-segment2)
    (into {})
    (vals)
    (apply +)))

