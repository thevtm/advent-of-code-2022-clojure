(ns advent-of-code-2022.day-06.solution)

(def input-text (slurp "./src/advent_of_code_2022/day_06/input.txt"))
;; (def input-text (slurp "./src/advent_of_code_2022/day_06/sample.txt"))

(defn first-index [pred coll]
  (first (keep-indexed #(when (pred %2) %1) coll)))

;; Answer 1: 1100
(->> input-text
     (partition 4 1)
     (first-index #(apply distinct? %))
     (+ 4))

;; Answer 2: 2421
(->> input-text
     (partition 14 1)
     (first-index #(apply distinct? %))
     (+ 14))
