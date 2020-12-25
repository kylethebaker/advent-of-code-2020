(ns aoc2020.day-16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;;------------------------------------------------
;; Shared
;;------------------------------------------------

(defn parse-note [note]
  (as-> note <>
    (re-find #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" <>)
    (rest <>)
    (let [field (first <>)
          [al ah bl bh] (map #(Integer/parseInt %) (rest <>))]
      [field [[al ah] [bl bh]]])))

(defn parse-notes [notes]
  (->> notes
    (str/split-lines)
    (map parse-note)
    (into {})))

(defn parse-ticket [t]
  (->> t
    (re-seq #"\d+")
    (map #(Integer/parseInt %))
    (vec)))

(defn parse-tickets [ts]
  (->> ts
    (str/split-lines)
    (rest)
    (map parse-ticket)))

(defn get-input []
  (as-> "./resources/day16.input" <>
    (slurp <>)
    (str/split <> #"\n\n")
    (let [[notes my-ticket nearby-tickets] <>]
      {:notes (parse-notes notes)
       :mine (first (parse-tickets my-ticket))
       :nearby (parse-tickets nearby-tickets)})))

(defn in-range [[low high] n]
  (<= low n high))

(defn in-ranges [ranges n]
  (some #(in-range % n) ranges))

(defn validator [notes]
  (as-> notes <>
    (vals <>)
    (map #(partial in-ranges %) <>)
    (apply juxt <>)
    #(some true? (<> %))))

;;------------------------------------------------
;; Part 1
;; Answer should be 20975
;;------------------------------------------------

(defn part-1 []
  (let [{:keys [notes nearby]} (get-input)
        valid? (validator notes)]
    (->> nearby
      (map #(remove valid? %))
      (flatten)
      (apply +))))

;;------------------------------------------------
;; Part 2
;; Answer should be 910339449193
;;------------------------------------------------

(defn with-idxs [xs]
  (map-indexed #(vector %1 %2) xs))

(defn departure-fields [notes]
  (->> (keys notes)
    (filter #(str/starts-with? % "departure"))
    (into #{})))

(defn remove-invalid [{:keys [notes nearby] :as input}]
  (let [valid? (validator notes)]
    (->> nearby
      (filter #(every? valid? %))
      (assoc-in input [:nearby]))))

(defn note-validators [notes]
  (->> notes
    (map (fn [[k v]] [k (partial in-ranges v)]))
    (into {})))

(defn join-valid-fields [acc [tkt field note]]
  (update-in acc [tkt field] set/union #{note}))

(defn join-same-cols [acc [col fields]]
  (update acc col conj fields))

(defn intersect-cols [valid-fields]
  (->> valid-fields
    (apply mapv vector)
    (mapcat identity)
    (reduce join-same-cols {})
    (map (fn [[col notes]] [col (apply set/intersection notes)]))
    (into {})))

(defn remove-field-from-others [col-notes skip-col field]
  (let [remove-field #(set/difference % #{field})]
    (reduce
      #(if-not (= %2 skip-col) (update %1 %2 remove-field) %1)
      col-notes
      (keys col-notes))))

(defn lock-in-single [[locked-in col-notes] [col fields]]
  (if (locked-in col)
    [locked-in col-notes]
    (if (= 1 (count fields))
      [(conj locked-in col)
       (remove-field-from-others col-notes col (first fields))]
      [locked-in col-notes])))

(defn are-fields-known? [fields col-notes]
  (->> col-notes
    (vals)
    (filter #(= 1 (count %)))
    (apply set/union)
    (set/intersection fields)
    (count)
    (= (count fields))))

(defn eliminate-until-known [fields-to-know col-notes]
  (loop [locked-in #{}, notes col-notes]
    (let [[locked-in notes]
          (reduce lock-in-single [locked-in notes] notes)]
      (if-not (are-fields-known? fields-to-know notes)
        (recur locked-in notes)
        notes))))

(defn valid-fields-for-tickets [note-vs tickets]
  (for [[tkt-i tkt] (with-idxs tickets)
        [pos v] (with-idxs tkt)
        [note valid?] note-vs
        :when (valid? v)]
    [tkt-i pos note]))

(defn extract-field-vals [fields col-notes]
  (for [[col notes] col-notes
        field fields
        :when (= 1 (count notes))
        :when (contains? notes field)]
    [col notes]))

(defn solve-for-fields [fields note-vs tickets]
  (->> tickets
    (valid-fields-for-tickets note-vs)
    (reduce join-valid-fields [])
    (intersect-cols)
    (eliminate-until-known fields)
    (extract-field-vals fields)
    (into {})))

(defn part-2 []
  (let [{:keys [notes mine nearby]} (get-input)
        depart-fields (departure-fields notes)
        note-vs (note-validators notes)
        tickets (concat [mine] nearby)
        departure-cols (solve-for-fields depart-fields note-vs tickets)]
    (->> departure-cols
      (keys)
      (map #(nth mine %))
      (apply *))))

