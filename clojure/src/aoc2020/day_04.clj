(ns aoc2020.day-04
  (:require [clojure.string :as str]))

(defn parse-entry [entry]
  (as-> entry _
    (str/split _ #"\s")
    (map #(str/split %1 #":") _)
    (into {} _)))

(defn get-input []
  (as-> "./resources/day04.input" _
    (slurp _)
    (str/split _ #"\n\n")
    (map parse-entry _)))

(defn only-missing-cid? [entry]
  (as-> entry _
    (dissoc _ "cid")
    (into #{} (keys _))
    (count _)
    (= 7 _)))

(defn get-rule [type]
  (case type
    "byr" #"19[2-9]\d|200[0-2]"
    "iyr" #"20(1\d|20)"
    "eyr" #"20(2\d|30)"
    "hgt" #"(1([5-8]\d|9[0-3])cm|(59|6\d|7[0-6])in)"
    "hcl" #"#[\da-f]{6}"
    "ecl" #"(amb|blu|brn|gry|grn|hzl|oth)"
    "pid" #"\d{9}"
    "cid" #".*"))

(defn valid-value? [[type v]]
  (-> type
    (get-rule)
    (re-matches v)
    (boolean)))

(defn valid-entry? [entry]
  (and
    (only-missing-cid? entry)
    (->> entry
      (seq)
      (map valid-value?)
      (every? true?))))

; Answer should be 192
(defn part-1 []
  (->> (get-input)
    (filter only-missing-cid?)
    (count)))

; Answer should be 101
(defn part-2 []
  (->> (get-input)
    (map valid-entry?)
    (filter true?)
    (count)))
