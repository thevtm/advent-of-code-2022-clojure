(ns advent-of-code-2022.day-03.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input-text (slurp "./src/advent_of_code_2022/day_03/input.txt"))
;; (def input-text (slurp "./src/advent_of_code_2022/day_03/sample.txt"))

(defn item->priority [item]
  (let [item-int (int item)
        is-lower-case (and (<= item-int 122) (>= item-int 97))
        item-case (if is-lower-case :lower :upper)
        character-offset (get {:lower (- (- (int \a) 1)) :upper (- (- (int \A) 1))} item-case)
        priority-offset (get {:lower 0 :upper 26} item-case)
        priority (+ item-int character-offset priority-offset)]
    priority))

;; Answer 1: 7817
(->> input-text
     (#(str/split % #"\n"))
     (map #(split-at (/ (count %) 2) %))
     (map #(map set %))
     (map #(set/intersection (first %) (second %)))
     (map #(item->priority (first %)))
     (reduce + 0))

;; Answer 2: 2444
(->> input-text
     (#(str/split % #"\n"))
     (partition 3)
     (map #(reduce set/intersection (map set %)))
     (map #(item->priority (first %)))
     (reduce + 0))
