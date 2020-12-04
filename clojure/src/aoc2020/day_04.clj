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

(defn to-num [s]
  (try (Integer/parseInt s)
    (catch Exception e nil)))

(defn year-in-range? [s min max]
  (if (re-matches #"[0-9]{4}" s)
    (if-let [n (to-num s)]
      (<= min n max))))

(defn valid-height? [v]
  (if-let [[_ height unit] (re-matches #"([0-9]+)(cm|in)" v)]
    (if-let [h (to-num height)]
      (case unit
        "cm" (<= 150 h 193)
        "in" (<= 59 h 76)))))

(defn valid-birth-year? [v]
  (year-in-range? v 1920 2002))

(defn valid-issue-year? [v]
  (year-in-range? v 2010 2020))

(defn valid-exp-year? [v]
  (year-in-range? v 2020 2030))

(defn valid-eye-color? [v]
  (some #(= %1 v) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))

(defn valid-hair-color? [v]
  (boolean (re-matches #"#[0-9a-f]{6}" v)))

(defn valid-passport-id? [v]
  (boolean (re-matches #"[0-9]{9}" v)))

(defn valid-value? [[type v]]
  (case type
    "byr" (valid-birth-year? v)
    "iyr" (valid-issue-year? v)
    "eyr" (valid-exp-year? v)
    "hgt" (valid-height? v)
    "hcl" (valid-hair-color? v)
    "ecl" (valid-eye-color? v)
    "pid" (valid-passport-id? v)
    "cid" true))

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
