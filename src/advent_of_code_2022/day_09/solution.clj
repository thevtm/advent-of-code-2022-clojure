(ns advent-of-code-2022.day-09.solution
  (:require [clojure.string :as str]))

(def input-text (slurp "./src/advent_of_code_2022/day_09/input.txt"))
;; (def input-text (slurp "./src/advent_of_code_2022/day_09/sample.txt"))
;; (def input-text (slurp "./src/advent_of_code_2022/day_09/sample-part-2.txt"))

;; Constants

(def starting-position '(0 0))

(def head-movement-map {:up '(0 1) :right '(1 0) :down '(0 -1) :left '(-1 0)})

(def tail-movement-map {'(2  0) '(1  0)
                        '(-2 0) '(-1 0)

                        '(0  2) '(0  1)
                        '(0 -2) '(0 -1)

                        '(2   1) '(1   1)
                        '(-2  1) '(-1  1)
                        '(2  -1) '(1  -1)
                        '(-2 -1) '(-1 -1)

                        '(1   2) '(1   1)
                        '(1  -2) '(1  -1)
                        '(-1  2) '(-1  1)
                        '(-1 -2) '(-1 -1)

                        '(2   2) '(1   1)
                        '(-2  2) '(-1  1)
                        '(2  -2) '(1  -1)
                        '(-2 -2) '(-1 -1)})

;; Vec2D

(defn vec2d-sum [a b] (map + a b))
(comment (vec2d-sum '(1 2) '(3 -4)))

(defn vec2d-minus [a b] (map - a b))
(comment (vec2d-minus '(1 2) '(3 -4)))

;; Lines

(def lines (str/split-lines input-text))

(defn line-direction [line]
  (get {\U :up \R :right \D :down \L :left} (first line)))
(comment (map list lines (map line-direction lines)))

(defn line-num-steps [line]
  (read-string (str/join (drop 2 line))))
(comment (map list lines (map line-num-steps lines)))

;; Steps

;; Step numbers are required to compute previous step
(def steps (->> lines
                (map #(repeat (line-num-steps %) (lines-direction %)))
                (flatten)
                (map list (range))
                (vec)))

(defn step-number [step] (first step))
(defn step-direction [step] (second step))

(defn step-previous [step] (nth steps (dec (step-number step)) nil))
(comment (map list steps (map step-previous steps)))

(def step-head-position
  (memoize
   (fn [step]
     (let [previous-step (step-previous step)
           previous-head-position (if (nil? previous-step) starting-position
                                      (step-head-position previous-step))]
       (vec2d-sum previous-head-position
                  (get head-movement-map (step-direction step)))))))
(comment (map list steps (map step-head-position steps)))

(def step-tail-position
  (memoize
   (fn [step-follower-position-fn step]
     (let [previous-step (step-previous step)

           previous-tail-position
           (if (nil? previous-step) starting-position
               (step-tail-position step-follower-position-fn previous-step))

           position-difference (vec2d-minus (step-follower-position-fn step)
                                            previous-tail-position)]
       (vec2d-sum previous-tail-position
                  (get tail-movement-map position-difference '(0 0)))))))
(comment (map list steps
              (map step-head-position steps)
              (map (partial step-tail-position step-head-position) steps)))

;; Answer 1: 6090
(->> (map (partial step-tail-position step-head-position) steps)
     (distinct)
     (count))

;; Answer 2: 2566
(->> (take 10 (iterate #(partial step-tail-position %) step-head-position))
     (last)
     (#(map % steps))
     (distinct)
     (count))
