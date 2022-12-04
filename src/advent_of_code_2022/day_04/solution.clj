(ns advent-of-code-2022.day-04.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input-text (slurp "./src/advent_of_code_2022/day_04/input.txt"))
;; (def input-text (slurp "./src/advent_of_code_2022/day_04/sample.txt"))

(defn parse-range [input-range]
  (let [input-range-splitted (str/split input-range #"-")
        range-min (read-string (first input-range-splitted))
        range-max (inc (read-string (second input-range-splitted)))]
    (range range-min range-max)))

(defn parse-line [input-line]
  (let [input-pairs (str/split input-line #",")
        range-a (set (parse-range (first input-pairs)))
        range-b (set (parse-range (second input-pairs)))]
    [range-a range-b]))

(defn has-superset? [range-a range-b]
  (let [is-range-a-superset-of-b (set/superset? range-a range-b)
        is-range-b-superset-of-a (set/superset? range-b range-a)
        is-superset (or is-range-a-superset-of-b is-range-b-superset-of-a)]
    is-superset))

;; Answer 1: 534
(->> input-text
     (str/split-lines)
     (map parse-line)
     (map #(has-superset? (first %) (second %)))
     (map #(get {false 0, true 1} %))
     (reduce + 0))

;; Answer 2: 841
(->> input-text
     (str/split-lines)
     (map parse-line)
     (map #(set/intersection (first %) (second %)))
     (map (complement empty?))
     (map #(get {false 0, true 1} %))
     (reduce + 0))

;; Using `set` to store the information abount the sectors is wasteful.
;; Ideally I'd have used a contruct to represent boundaries but AFAIK Clojure's
;; standard library doesn't have one.
