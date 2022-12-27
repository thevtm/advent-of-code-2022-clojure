(ns advent-of-code-2022.day-08.solution
  (:require [datascript.core :as d]
            [clojure.string :as str]))

(def input-text (slurp "./src/advent_of_code_2022/day_08/input.txt"))
(def sample-text (slurp "./src/advent_of_code_2022/day_08/sample.txt"))

(def conn (d/create-conn))

;; Insert inputs
(d/transact! conn [{:input/text input-text
                    :input/name "input"}
                   {:input/text sample-text
                    :input/name "sample"}])

;; Split Lines
(->> (d/q '[:find ?input-entity ?input-text
            :where
            [?input-entity :input/text ?input-text]
            [?input-entity :input/name "input"]]
          @conn)
     (map
      (fn [[input-entity-id input-text]]
        (map (fn [line-number line-text] (hash-map :input-line/number line-number
                                                   :input-line/text line-text
                                                   :depend-on input-entity-id))
             (range 1 ##Inf) (str/split-lines input-text))))
     (flatten)
     (d/transact! conn))

;; Parse trees
(->> (d/q '[:find ?input-line-text ?input-line-number
            :where
            [?input-line-eid :input-line/text ?input-line-text]
            [?input-line-eid :input-line/number ?input-line-number]]
          @conn)
     (map
      (fn [[?input-line-text ?input-line-number]]
        (map-indexed (fn [index input-line-character]
                       (hash-map :tree/height (- (int input-line-character) 48)
                                 :tree/position-x (inc index)
                                 :tree/position-y ?input-line-number))
                     ?input-line-text)))
     (flatten)
     (d/transact! conn))

;; Norh
(->> (d/q '[:find ?tree-eid ?tree-north-eid
            :where
            [?tree-eid :tree/position-x ?tree-position-x]
            [?tree-eid :tree/position-y ?tree-position-y]
            [(dec ?tree-position-y) ?tree-north-position-y]
            [?tree-north-eid :tree/position-x ?tree-position-x]
            [?tree-north-eid :tree/position-y ?tree-north-position-y]]
          @conn)
     (map (fn [[?tree-eid ?tree-north-eid]]
            (hash-map :db/id ?tree-eid
                      :tree/northern-tree ?tree-north-eid)))
     (d/transact! conn))

;; South
(->> (d/q '[:find ?tree-eid ?tree-south-eid
            :where
            [?tree-eid :tree/position-x ?tree-position-x]
            [?tree-eid :tree/position-y ?tree-position-y]
            [(inc ?tree-position-y) ?tree-south-position-y]
            [?tree-south-eid :tree/position-x ?tree-position-x]
            [?tree-south-eid :tree/position-y ?tree-south-position-y]]
          @conn)
     (map (fn [[?tree-eid ?tree-south-eid]]
            (hash-map :db/id ?tree-eid
                      :tree/southern-tree ?tree-south-eid)))
     (d/transact! conn))

;; West
(->> (d/q '[:find ?tree-eid ?tree-west-eid
            :where
            [?tree-eid :tree/position-x ?tree-position-x]
            [?tree-eid :tree/position-y ?tree-position-y]
            [(dec ?tree-position-x) ?tree-west-position-x]
            [?tree-west-eid :tree/position-x ?tree-west-position-x]
            [?tree-west-eid :tree/position-y ?tree-position-y]]
          @conn)
     (map (fn [[?tree-eid ?tree-west-eid]]
            (hash-map :db/id ?tree-eid
                      :tree/western-tree ?tree-west-eid)))
     (d/transact! conn))

;; East
(->> (d/q '[:find ?tree-eid ?tree-east-eid
            :where
            [?tree-eid :tree/position-x ?tree-position-x]
            [?tree-eid :tree/position-y ?tree-position-y]
            [(inc ?tree-position-x) ?tree-east-position-x]
            [?tree-east-eid :tree/position-x ?tree-east-position-x]
            [?tree-east-eid :tree/position-y ?tree-position-y]]
          @conn)
     (map (fn [[?tree-eid ?tree-east-eid]]
            (hash-map :db/id ?tree-eid
                      :tree/eastern-tree ?tree-east-eid)))
     (d/transact! conn))

;; Visible from the North
(->> (d/q '[:find ?tree-eid
            :in $
            :where
            [?tree-eid :tree/height _]
            [(missing? $ ?tree-eid :tree/northern-tree)]]
          @conn)
     (map (fn [[?tree-eid]]
            (hash-map :db/id ?tree-eid
                      :tree/visible-from-the-north true)))
     (d/transact! conn))

(->> (d/q '[:find ?tree-eid
            :where
            [?tree-eid :tree/northern-tree _]]
          @conn)
     (map (fn [[?tree-eid]]
            (let [tree (d/entity @conn ?tree-eid)
                  tree-height (:tree/height tree)
                  is-visible (->> tree
                                  (iterate #(d/entity @conn (:tree/northern-tree %)))
                                  (drop 1)
                                  (take-while some?)
                                  (some #(<= tree-height (:tree/height %)))
                                  (not))]
              (hash-map :db/id ?tree-eid
                        :tree/visible-from-the-north is-visible))))
     (d/transact! conn))

;; Visible from the South
(->> (d/q '[:find ?tree-eid
            :in $
            :where
            [?tree-eid :tree/height _]
            [(missing? $ ?tree-eid :tree/southern-tree)]]
          @conn)
     (map (fn [[?tree-eid]]
            (hash-map :db/id ?tree-eid
                      :tree/visible-from-the-south true)))
     (d/transact! conn))

(->> (d/q '[:find ?tree-eid
            :where
            [?tree-eid :tree/southern-tree _]]
          @conn)
     (map (fn [[?tree-eid]]
            (let [tree (d/entity @conn ?tree-eid)
                  tree-height (:tree/height tree)
                  is-visible (->> tree
                                  (iterate #(d/entity @conn (:tree/southern-tree %)))
                                  (drop 1)
                                  (take-while some?)
                                  (some #(<= tree-height (:tree/height %)))
                                  (not))]
              (hash-map :db/id ?tree-eid
                        :tree/visible-from-the-south is-visible))))
     (d/transact! conn))

;; Visible from the West
(->> (d/q '[:find ?tree-eid
            :in $
            :where
            [?tree-eid :tree/height _]
            [(missing? $ ?tree-eid :tree/western-tree)]]
          @conn)
     (map (fn [[?tree-eid]]
            (hash-map :db/id ?tree-eid
                      :tree/visible-from-the-west true)))
     (d/transact! conn))

(->> (d/q '[:find ?tree-eid
            :where
            [?tree-eid :tree/western-tree _]]
          @conn)
     (map (fn [[?tree-eid]]
            (let [tree (d/entity @conn ?tree-eid)
                  tree-height (:tree/height tree)
                  is-visible (->> tree
                                  (iterate #(d/entity @conn (:tree/western-tree %)))
                                  (drop 1)
                                  (take-while some?)
                                  (some #(<= tree-height (:tree/height %)))
                                  (not))]
              (hash-map :db/id ?tree-eid
                        :tree/visible-from-the-west is-visible))))
     (d/transact! conn))

;; Visible from the East
(->> (d/q '[:find ?tree-eid
            :in $
            :where
            [?tree-eid :tree/height _]
            [(missing? $ ?tree-eid :tree/eastern-tree)]]
          @conn)
     (map (fn [[?tree-eid]]
            (hash-map :db/id ?tree-eid
                      :tree/visible-from-the-east true)))
     (d/transact! conn))

(->> (d/q '[:find ?tree-eid
            :where
            [?tree-eid :tree/eastern-tree _]]
          @conn)
     (map (fn [[?tree-eid]]
            (let [tree (d/entity @conn ?tree-eid)
                  tree-height (:tree/height tree)
                  is-visible (->> tree
                                  (iterate #(d/entity @conn (:tree/eastern-tree %)))
                                  (drop 1)
                                  (take-while some?)
                                  (some #(<= tree-height (:tree/height %)))
                                  (not))]
              (hash-map :db/id ?tree-eid
                        :tree/visible-from-the-east is-visible))))
     (d/transact! conn))

;; Answer 1: 1805
(->> (d/q '[:find ?tree-eid
            :where
            [?tree-eid :tree/height ?tree-height]
            [?tree-eid :tree/visible-from-the-north ?visible-north]
            [?tree-eid :tree/visible-from-the-south ?visible-south]
            [?tree-eid :tree/visible-from-the-west ?visible-west]
            [?tree-eid :tree/visible-from-the-east ?visible-east]
            [(or ?visible-north ?visible-south ?visible-west ?visible-east) ?visible]
            [(true? ?visible)]]
          @conn)
     (count))

;; Scenic Score
(defn take-while+
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

;; Answer 2: 444528
(->> (d/q '[:find ?tree-eid
            :where
            [?tree-eid :tree/height _]]
          @conn)
     (map (fn [[?tree-eid]]
            (let [tree (d/entity @conn ?tree-eid)
                  tree-height (:tree/height tree)
                  calculate-scenic-score  (fn [direction-key]
                                            (->> tree
                                                 (iterate #(d/entity @conn (direction-key %)))
                                                 (drop 1)
                                                 (take-while some?)
                                                 (take-while+ #(> tree-height (:tree/height %)))
                                                 (count)))
                  scenic-score-north (calculate-scenic-score :tree/northern-tree)
                  scenic-score-south (calculate-scenic-score :tree/southern-tree)
                  scenic-score-west (calculate-scenic-score :tree/western-tree)
                  scenic-score-east (calculate-scenic-score :tree/eastern-tree)
                  scenic-score (* scenic-score-north scenic-score-south scenic-score-west scenic-score-east)]
              (hash-map :x (:tree/position-x tree)
                        :y (:tree/position-y tree)
                        :tree/height (:tree/height tree)
                        :scenic-score-north scenic-score-north
                        :scenic-score-south scenic-score-south
                        :scenic-score-west scenic-score-west
                        :scenic-score-east scenic-score-east
                        :scenic-score scenic-score))))
     (map :scenic-score)
     (apply max))

(d/datoms @conn :eavt 3)
