(ns advent-of-code-2022.day-05.solution
  (:require [clojure.string :as str]))

(def input-text (slurp "./src/advent_of_code_2022/day_05/input.txt"))
;; (def input-text (slurp "./src/advent_of_code_2022/day_05/sample.txt"))

(defn parse-crates-line [crate-line]
  (let [crate-line-padded (str/join (concat crate-line " "))
        crate-chars (map second (partition 4 crate-line-padded))
        crate-chars-zipped (zipmap (range 1 ##Inf) crate-chars)]
    (filter #(not (= \space (second %))) crate-chars-zipped)))

(defn parse-crates [crate-lines]
  (let [crate-lines-parsed (map parse-crates-line crate-lines)
        crates-sequence (apply concat (reverse crate-lines-parsed))]
    (reduce (fn [acc x] (update-in acc [(first x)] #(vec (concat [(second x)] %))))
            {} crates-sequence)))

(def crates-initial-state
  (->> input-text
       (#(str/split % #"\n\n"))
       (first)
       (str/split-lines)
       (butlast)
       (parse-crates)))

(def move-commands
  (->> input-text
       (#(str/split % #"\n\n"))
       (second)
       (re-seq #"\d+")
       (map read-string)
       (partition 3)))

;; Answer 1: NTWZZWHFV
(->> move-commands
     (map #(repeat (first %) (rest %)))
     (apply concat)
     (reduce (fn [crates-state move-command]
               (let [source-pile (first move-command)
                     target-pile (second move-command)
                     crate-removed (first (get crates-state source-pile))
                     crates-state-without-removed-crate (update-in crates-state [source-pile] #(vec (rest %)))
                     crates-state-next (update-in crates-state-without-removed-crate [target-pile] #(vec (concat [crate-removed] %)))]
                 crates-state-next))
             crates-initial-state)
     (seq)
     (sort-by #(first %))
     (map #(first (second %)))
     (str/join))

;; Answer 2: BRZGFVBTJ
(->> move-commands
     (reduce (fn [crates-state move-command]
               (let [quantity (nth move-command 0)
                     source-pile (nth move-command 1)
                     target-pile (nth move-command 2)
                     crate-removed (take quantity (get crates-state source-pile))
                     crates-state-without-removed-crate (update-in crates-state [source-pile] #(vec (drop quantity %)))
                     crates-state-next (update-in crates-state-without-removed-crate [target-pile] #(vec (concat crate-removed %)))]
                 crates-state-next))
             crates-initial-state)
     (seq)
     (sort-by #(first %))
     (map #(first (second %)))
     (str/join))
