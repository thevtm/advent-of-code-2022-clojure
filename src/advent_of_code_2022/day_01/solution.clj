(ns advent-of-code-2022.day-01.solution
  (:require [clojure.string :refer :all]
            [clojure.edn :as edn]))

(def input-text (slurp "./src/advent_of_code_2022/day_01/input.txt"))

(def input-parsed (map #(map edn/read-string (split % #"\n"))
                       (split input-text #"\n\n")))

(def total-calories-per-elf (map #(reduce + 0 %) input-parsed))

;; Answer 1: 72017
(apply max total-calories-per-elf)

;; Answer 2: 212520
(reduce + 0 (take-last 3 (sort total-calories-per-elf)))
