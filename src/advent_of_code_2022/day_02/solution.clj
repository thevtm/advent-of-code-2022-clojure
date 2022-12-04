(ns advent-of-code-2022.day-02.solution 
  (:require [clojure.string :as str]))

(def input-text (slurp "./src/advent_of_code_2022/day_02/input.txt"))
;; (def input-text (slurp "./src/advent_of_code_2022/day_02/sample.txt"))

(def my-pick-map {"X" :rock
                  "Y" :paper
                  "Z" :scissors})

(def opponents-pick-map {"A" :rock
                         "B" :paper
                         "C" :scissors})

(def results-map {{:my-pick :rock :opponents-pick :rock} :draw
                  {:my-pick :rock :opponents-pick :paper} :lost
                  {:my-pick :rock :opponents-pick :scissors} :win
                  {:my-pick :paper :opponents-pick :rock} :win
                  {:my-pick :paper :opponents-pick :paper} :draw
                  {:my-pick :paper :opponents-pick :scissors} :lost
                  {:my-pick :scissors :opponents-pick :rock} :lost
                  {:my-pick :scissors :opponents-pick :paper} :win
                  {:my-pick :scissors :opponents-pick :scissors} :draw})

(def shape-score-map {:rock 1
                      :paper 2
                      :scissors 3})

(def result-score-map {:lost 0
                       :draw 3
                       :win 6})

(defn parse-play [play-text]
  (let [play-text-splitted (str/split play-text #" ")
        my-pick (get my-pick-map (second play-text-splitted))
        opponents-pick (get opponents-pick-map (first play-text-splitted))
        result (get results-map {:my-pick my-pick :opponents-pick opponents-pick})
        shape-score (get shape-score-map my-pick)
        result-score (get result-score-map result)
        score (+ shape-score result-score)]
    {:my-pick my-pick
     :opponents-pick opponents-pick
     :result result
     :shape-score shape-score
     :result-score result-score
     :score score}))

;; Answer 1: 10816
(->> input-text 
     (#(str/split % #"\n"))
     (map parse-play)
     (map #(get % :score))
     (reduce + 0))

;; Answer 2: 11657
(->> input-text
     (#(str/split % #"\n"))
     (map #(get {"A X" (+ (get result-score-map :lost) (get shape-score-map :scissors))
                 "B X" (+ (get result-score-map :lost) (get shape-score-map :rock))
                 "C X" (+ (get result-score-map :lost) (get shape-score-map :paper))
                 "A Y" (+ (get result-score-map :draw) (get shape-score-map :rock))
                 "B Y" (+ (get result-score-map :draw) (get shape-score-map :paper))
                 "C Y" (+ (get result-score-map :draw) (get shape-score-map :scissors))
                 "A Z" (+ (get result-score-map :win) (get shape-score-map :paper))
                 "B Z" (+ (get result-score-map :win) (get shape-score-map :scissors))
                 "C Z" (+ (get result-score-map :win) (get shape-score-map :rock))}
                %))
     (reduce + 0))
