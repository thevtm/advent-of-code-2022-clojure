(ns advent-of-code-2022.day-11.solution
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def input-text (slurp "./src/advent_of_code_2022/day_11/input.txt"))
(def input-text (slurp "./src/advent_of_code_2022/day_11/sample.txt"))

;; Helpers

(defn divisible? [n d] (= (mod n d) 0))

;; Lines

(def lines (str/split-lines input-text))

(def monkeys (partition-all 7 lines))

(defn monkey-num [monkey]
  (if (nil? monkey) nil
      (read-string (str (nth (nth monkey 0) 7)))))
(comment (map list monkeys (map monkey-num monkeys)))

(defn num-monkey [num]
  (nth monkeys num nil))
(comment (map list
              (map monkey-num monkeys)
              (map num-monkey (map monkey-num monkeys))))

(defn monkey-previous [monkey] (nth monkeys (dec (monkey-num monkey)) nil))
(comment (map list monkeys (map monkey-previous monkeys)))

(defn monkey-start-items [monkey]
  (map read-string (str/split (str/join (drop 18 (nth monkey 1))) #", ")))
(comment (map list monkeys (map monkey-start-items monkeys)))

(defn monkey-operation-operator [monkey]
  (str (nth (nth monkey 2) 23)))
(comment (map list monkeys (map monkey-operation-operator monkeys)))

(defn monkey-operation-operator-fn [monkey]
  (case (monkey-operation-operator monkey)
    "+" +
    "*" *))

(defn monkey-operation-argument [monkey]
  (read-string (str/join (drop 25 (nth monkey 2)))))
(comment (map list monkeys (map monkey-operation-argument monkeys)))

(defn monkey-test-argument [monkey]
  (read-string (str/join (drop 21 (nth monkey 3)))))
(comment (map list monkeys (map monkey-test-argument monkeys)))

(defn monkey-test-when-true-throw-to [monkey]
  (read-string (str/join (drop 29 (nth monkey 4)))))
(comment (map list monkeys (map monkey-test-when-true-throw-to monkeys)))

(defn monkey-test-when-false-throw-to [monkey]
  (read-string (str/join (drop 30 (nth monkey 5)))))
(comment (map list monkeys (map monkey-test-when-false-throw-to monkeys)))

(defn monkey-throw-item-new-worry-level [monkey item]
  (let [argument (monkey-operation-argument monkey)
        operation-fn (case (monkey-operation-operator monkey)
                       "+" +
                       "*" *)]
    (if (= (monkey-operation-argument monkey) 'old)
      (quot (operation-fn item item) 3)
      (quot (operation-fn item argument) 3))))
(comment (monkey-throw-item-new-worry-level (nth monkeys 0) 79))
(comment (monkey-throw-item-new-worry-level (nth monkeys 2) 74))

(defn monkey-throw-item [monkey item]
  (if (divisible? (monkey-throw-item-new-worry-level monkey item)
                  (monkey-test-argument monkey))
    (monkey-test-when-true-throw-to monkey)
    (monkey-test-when-false-throw-to monkey)))
(comment (monkey-throw-item (nth monkeys 0) 79))
(comment (monkey-throw-item (nth monkeys 2) 60))

(defn round-monkey-previous [round monkey]
  (if (nil? (monkey-previous monkey))
    [(dec round) (last monkeys)]
    [round (monkey-previous monkey)]))
(comment (map list monkeys (map #(round-monkey-previous 0 %) monkeys)))

(def monkeys-initial-items
  (zipmap (range)
          (map #(hash-map :items (monkey-start-items %)
                          :count 0)
               monkeys)))

(defn round-items [round monkey]
  (if (< round 0)
    monkeys-initial-items
    (let [previous-round-items (apply round-items (round-monkey-previous round monkey))
          previous-round-items-with-current-monkey-items (assoc-in previous-round-items [(monkey-num monkey) :items] '())
          monkey-items (get-in previous-round-items [(monkey-num monkey) :items])
          previous-round-items-with-current-monkey-items-and-count (update-in previous-round-items-with-current-monkey-items [(monkey-num monkey) :count] + (count monkey-items))]
      (reduce (fn [acc monkey-item]
                (let [monkey-item-thrown-to (monkey-throw-item monkey monkey-item)
                      monkey-item-thrown-worry-level (monkey-throw-item-new-worry-level monkey monkey-item)]
                  (update-in acc [monkey-item-thrown-to :items] #(concat % [monkey-item-thrown-worry-level]))))
              previous-round-items-with-current-monkey-items-and-count monkey-items))))
(comment (round-items 19 (last monkeys)))

;; Answer 1: 76728
(->> (round-items 19 (last monkeys))
     (vals)
     (map :count)
     (sort)
     (reverse)
     (take 2)
     (apply *))

;; Part 2

(def rounds (range 40))
(comment rounds)

(def round-previous (fn [round] (if (<= round 0) nil (dec round))))
(comment (map list rounds (map round-previous rounds)))

(defn monkey-test-when-true-throw-to [monkey]
  (num-monkey (read-string (str/join (drop 29 (nth monkey 4))))))
(comment (map list monkeys (map monkey-test-when-true-throw-to monkeys)))

(defn monkey-test-when-false-throw-to [monkey]
  (num-monkey (read-string (str/join (drop 30 (nth monkey 5))))))
(comment (map list monkeys (map monkey-test-when-false-throw-to monkeys)))

(def round-monkeys (combo/cartesian-product rounds monkeys))
(comment round-monkeys)
(comment (map #(vector (first %) (monkey-num (last %))) round-monkeys))

(defn round-monkey-round [round-monkey]
  (first round-monkey))

(defn round-monkey-monkey [round-monkey]
  (last round-monkey))

(defn round-monkey-previous [[round monkey]]
  (if-some [previous-monkey (monkey-previous monkey)]
    [round previous-monkey]
    (when-some [previous-round (round-previous round)]
      [previous-round (last monkeys)])))
(comment (map list
              (map #(vector (first %) (monkey-num (last %))) round-monkeys)
              (map #(if (nil? %) nil (vector (first %) (monkey-num (last %))))
                   (map round-monkey-previous round-monkeys))))

(defn round-monkey-items [[round monkey]]
  (cond
    (zero? round) (monkey-start-items monkey)
    :else nil))
(comment (map list
              (map #(vector (first %) (monkey-num (last %))) round-monkeys)
              (map round-monkey-items round-monkeys)))

(defn round-monkey-inspection-round-monkey [round-monkeys-inspection]
  (first round-monkeys-inspection))

(defn round-monkey-inspection-inspection [round-monkeys-inspection]
  (second round-monkeys-inspection))

(defn round-monkey-inspection-previous [[round-monkeys inspection]]
  (if (zero? inspection) nil
      [round-monkeys (dec inspection)]))
(comment (round-monkey-inspection-previous [(first round-monkeys) 0]))

(defn round-monkey-inspection-operation-result [round-monkey-inspection]
  (let [round-monkey (round-monkey-inspection-round-monkey round-monkey-inspection)
        monkey (round-monkey-monkey round-monkey)
        item (round-monkey-inspection-selected-item round-monkey-inspection)
        argument (monkey-operation-argument monkey)
        operation-fn (monkey-operation-operator-fn monkey)
        second-argument (if (= (monkey-operation-argument monkey) 'old) item argument)]
    (mod (operation-fn item second-argument) (monkey-test-argument monkey))))
(comment (round-monkey-inspection-operation-result [(first round-monkeys) 0]))

(defn round-monkey-inspection-test-result [round-monkey-inspection]
  (let [round-monkey (round-monkey-inspection-round-monkey round-monkey-inspection)
        monkey (round-monkey-monkey round-monkey)
        operation-result (round-monkey-inspection-operation-result round-monkey-inspection)]
    (divisible? operation-result (monkey-test-argument monkey))))
(comment (round-monkey-inspection-test-result [(first round-monkeys) 0]))

(defn round-monkey-inpection-throw-to [round-monkey-inspection]
  (let [round-monkey (round-monkey-inspection-round-monkey round-monkey-inspection)]
    (if (round-monkey-inspection-test-result round-monkey-inspection)
      (monkey-test-when-true-throw-to (round-monkey-monkey round-monkey))
      (monkey-test-when-false-throw-to (round-monkey-monkey round-monkey)))))
(comment (round-monkey-inpection-throw-to [(first round-monkeys) 0]))

(defn round-monkey-inspection-monkey-items [[round-monkey-inspection monkey]]
  (let [round-monkey (round-monkey-inspection-round-monkey round-monkey-inspection)
        round-monkey-round (round-monkey-round round-monkey)
        inspecting-monkey (round-monkey-monkey round-monkey)
        inspection (round-monkey-inspection-inspection round-monkey-inspection)

        is-first-round? (= round-monkey-round 0)
        is-first-monkey? (= inspecting-monkey (first monkeys))
        is-first-inspection? (= inspection 0)]
    (if (and is-first-round? is-first-monkey? is-first-inspection?)
      (monkey-start-items (first monkeys))
      (let [previous-inspection (round-monkey-inspection-previous round-monkey-inspection)
            previous-inspection-round-monkey (round-monkey-inspection-round-monkey previous-inspection)
            previous-inspection-monkey-items (round-monkey-inspection-monkey-items [previous-inspection monkey])
            previous-inspection-inspecting-monkey (round-monkey-monkey previous-inspection-round-monkey)
            previous-inspection-monkey-thrown-to (round-monkey-inpection-throw-to previous-inspection)
            previous-inspection-monkey-selected (round-monkey-inspection-selected-item previous-inspection)

            is-previous-inspection-inspecting-monkey? (= monkey previous-inspection-inspecting-monkey)
            is-previous-inspection-monkey-to-throw? (= previous-inspection-monkey-thrown-to monkey)]
        (cond
          is-previous-inspection-inspecting-monkey? (rest previous-inspection-monkey-items)
          is-previous-inspection-monkey-to-throw? (cons previous-inspection-monkey-items previous-inspection-monkey-selected)
          :else nil)))))
(comment (round-monkey-inspection-monkey-items
          [[(first round-monkeys) 0] (first monkeys)]))
(comment (round-monkey-inspection-monkey-items
          [[(first round-monkeys) 0] (nth monkeys 2)]))
(comment (round-monkey-inspection-monkey-items
          [[(first round-monkeys) 1] (first monkeys)]))


(defn round-monkey-inspection-selected-item [round-monkey-inspection]
  (let [round-monkey (round-monkey-inspection-round-monkey round-monkey-inspection)
        round-monkey-monkey (round-monkey-monkey round-monkey)]
    (first (round-monkey-inspection-monkey-items
            [round-monkey-inspection round-monkey-monkey]))))
(comment (round-monkey-inspection-selected-item
          [(first round-monkeys) 0]))


;; Don't divide by 3
(defn monkey-throw-item-new-worry-level [monkey item]
  (let [argument (monkey-operation-argument monkey)
        operation-fn (case (monkey-operation-operator monkey)
                       "+" +
                       "*" *)
        second-argument (if (= (monkey-operation-argument monkey) 'old) item argument)]
    (mod (operation-fn item second-argument) (monkey-test-argument monkey))))

(defn monkey-throw-item [monkey item]
  (if (= (monkey-throw-item-new-worry-level monkey item) 0)
    (monkey-test-when-true-throw-to monkey)
    (monkey-test-when-false-throw-to monkey)))

(defn round-items [round monkey]
  (loop [round monkey]
    (if (< round 0)
      monkeys-initial-items
      (let [previous-round-monkey (round-monkey-previous round monkey)
            previous-round-items (recur (first previous-round-monkey) (second previous-round-monkey))
            previous-round-items-with-current-monkey-items (assoc-in previous-round-items [(monkey-num monkey) :items] '())
            monkey-items (get-in previous-round-items [(monkey-num monkey) :items])
            previous-round-items-with-current-monkey-items-and-count (update-in previous-round-items-with-current-monkey-items [(monkey-num monkey) :count] + (count monkey-items))]
        (reduce (fn [acc monkey-item]
                  (let [monkey-item-thrown-to (monkey-throw-item monkey monkey-item)
                        monkey-item-thrown-worry-level (monkey-throw-item-new-worry-level monkey monkey-item)]
                    (update-in acc [monkey-item-thrown-to :items] #(concat % [monkey-item-thrown-worry-level]))))
                previous-round-items-with-current-monkey-items-and-count monkey-items)))))

(comment (round-items 1 (last monkeys)))

;; Answer 2:
(->> (round-items 40 (last monkeys))
     (vals)
     (map :count)
     (sort)
     (reverse)
     (take 2)
     (apply *))
