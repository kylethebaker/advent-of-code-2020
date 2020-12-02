(ns aoc2020.day-01
  (:require [clojure.math.combinatorics :as cmb]))

(defn get-expenses []
  (->> "./resources/day01.input"
    (slurp)
    (re-seq #"\d+")
    (map #(Integer/parseInt %))))

(defn apply-with-key [entry k fn]
    (assoc entry k (apply fn (:items entry))))

(defn with-sum [e] (apply-with-key e :sum +))
(defn with-product [e] (apply-with-key e :product *))

(defn find-product-of-2020 [entries]
  (->> entries
    (map #(hash-map :items %))
    (map with-sum)
    (filter #(= 2020 (:sum %)))
    (map with-product)
    (first)
    (:product)))

;; Answer should be 713184
(defn part-1 []
  (-> (get-expenses)
    (combinations 2)
    (find-product-of-2020)))

;; Answer should be 261244452
(defn part-2 []
  (-> (get-expenses)
    (combinations 3)
    (find-product-of-2020)))
