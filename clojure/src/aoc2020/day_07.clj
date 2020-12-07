(ns aoc2020.day-07
  (:require [clojure.string :as str]
            [clojure.set :refer [union difference]]))

(defn parse-single-bag [c]
  (if (not= c "no other")
    (let [[n shade color] (str/split c #" ")]
      [[shade color] (Integer/parseInt n)])))
    {}

(defn parse-rule [[shade color contents]]
  (as-> contents _
    (str/split _ #" bags?(, )?")
    (map parse-single-bag _)
    (into {} _)
    [[shade color] _]))

(defn get-input []
  (->> "./resources/day07.input"
    (slurp)
    (re-seq #"(\w+) (\w+) bags contain (.*)\.\n")
    (map rest)
    (map parse-rule)
    (into {})))

(defn add-bag-if-fits [rules bag-to-fit found bag-to-check]
  (if ((rules bag-to-check) bag-to-fit)
    (conj found bag-to-check)
    found))

(defn find-direct-fits [rules bag-to-fit]
  (->> rules
    (keys)
    (reduce #(add-bag-if-fits rules bag-to-fit %1 %2) #{})))

(defn find-all-fits [rules bag-to-fit]
  (loop [found #{} bags [bag-to-fit]]
    (as-> bags _
      (map #(find-direct-fits rules %1) _)
      (apply union _)
      (difference _ found)
      (let [fresh _]
        (if-not (empty? fresh)
          (recur (into found fresh) (into [] fresh))
          found)))))

(defn add-count-to-total [rules total bag n]
  (->> bag
    (count-child-bags rules)
    (* n)
    (+ total)))

(defn count-child-bags [rules bag]
  (let [children (rules bag)]
    (if-not (empty? children)
      (reduce-kv #(add-count-to-total rules %1 %2 %3) 1 children)
      1)))

; Answer should be 155
(defn part-1 []
  (-> (get-input)
    (find-all-fits ["shiny" "gold"])
    (count)))

; Answer should be 54083
(defn part-2 []
  (-> (get-input)
    (count-child-bags ["shiny" "gold"])
    (dec)))
