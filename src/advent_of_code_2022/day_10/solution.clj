(ns advent-of-code-2022.day-10.solution
  (:require [clojure.string :as str]))

(def input-text (slurp "./src/advent_of_code_2022/day_10/input.txt"))
;; (def input-text (slurp "./src/advent_of_code_2022/day_10/sample.txt"))

;; Lines

(def lines (str/split-lines input-text))

(defn line-operation [line] (if (= line "noop") :noop :addx))
(comment (map list lines (map line-operation lines)))

(defn addx-line-argument [line] (read-string (str/join (drop 5 line))))

(defn line-x-delta [line] (case (line-operation line)
                            :noop '(0)
                            :addx (list 0 (addx-line-argument line))))
(comment (map list lines (map line-x-delta lines)))

(def cycle-x-values (vec (reductions + (flatten (concat '(1 0) (map line-x-delta lines))))))

(comment (nth cycle-x-values 20))
(comment (drop 200 (map list (range) cycle-x-values)))

(comment (map (partial nth cycle-x-values) [20 60 100 140 180 220]))

;; Answer 1: 10760
(reduce + (map #(* % (nth cycle-x-values %)) [20 60 100 140 180 220]))

;; Answer 2: FPGPHFGH
(def cycle-x-value-sprite-x-positions
  (vec (map #(set [(dec %) % (inc %)]) (drop 1 cycle-x-values))))
(comment (drop 0 (map list (range) cycle-x-values cycle-x-value-sprite-x-positions)))

(def cycle-cursor-x-positions (map #(mod % 40) (range)))

(def screen-pixels
  (->> (map contains? cycle-x-value-sprite-x-positions cycle-cursor-x-positions)
       (map #(if % "#" "."))))

(->> screen-pixels
     (partition 40)
     (map str/join))
