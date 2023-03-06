(ns advent-of-code-2022.day-07.solution-datascript
  (:require [datascript.core :as d]
            [clojure.string :as str]))

;; Attempted to solve it using datascript
;; Even though it's possible, it's very verbose, slow and hard to debug
;; Gave up on it
;; 
;; Day 08 was solved using it

(def input-text (slurp "./src/advent_of_code_2022/day_07/input.txt"))
(def sample-text (slurp "./src/advent_of_code_2022/day_07/sample.txt"))


(def schema {:user/id {:db.unique :db.unique/identity}
             :user/name {}
             :user/age {}
             :user/parent {:db.valueType :db.type/ref
                           :db.cardinality :db.cardinality/many}})

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
            [?input-entity :input/name "sample"]]
          @conn)
     (map
      (fn [[input-entity-id input-text]]
        (map (fn [line-number line-text] (hash-map :input-line/number line-number
                                                   :input-line/text line-text
                                                   :depend-on input-entity-id))
             (range 1 ##Inf) (str/split-lines input-text))))
     (flatten)
     (d/transact! conn))

;; Add property input-line/next and input-line/previous to input-line
;; (this could've been done when the lines were added using negative ids as references)
(->> (d/q '[:find ?previous-input-line-eid ?next-input-line-eid
            :where
            [?previous-input-line-eid :input-line/number ?previous-input-line-number]
            [?next-input-line-eid :input-line/number ?next-input-line-number]
            [(inc ?previous-input-line-number) ?next-input-line-number]]
          @conn)
     (map (fn [[?previous-input-line-eid ?next-input-line-eid]]
            [(hash-map :db/id ?previous-input-line-eid
                       :input-line/next ?next-input-line-eid)
             (hash-map :db/id ?next-input-line-eid
                       :input-line/previous ?previous-input-line-eid)]))
     (flatten)
     (d/transact! conn))

;; Parse input-line
(->> (d/q '[:find ?input-line-eid ?input-line-text
            :where
            [?input-line-eid :input-line/text ?input-line-text]]
          @conn)
     (map (fn [[?input-line-eid ?input-line-text]]
            (let [type (if (str/starts-with? ?input-line-text "$")
                         :input-line/command :input-line/output)
                  is-command? (= type :input-line/command)
                  is-output? (= type :input-line/output)
                  is-ls-output? is-output?
                  command (when is-command? (str/join (take 2 (drop 2 ?input-line-text))))
                  is-command-cd? (= command "cd")
                  is-command-ls? (= command "ls")
                  command-cd-arg (when is-command-cd? (str/join (drop 5 ?input-line-text)))
                  [file-size-text file-name] (when is-ls-output? (str/split ?input-line-text #" "))
                  file-size (when file-size-text (read-string file-size-text))]
              (cond is-command-cd? (hash-map :db/id ?input-line-eid
                                             :input-line/type type
                                             :command/name command
                                             :command/arg command-cd-arg)
                    is-command-ls? (hash-map :db/id ?input-line-eid
                                             :input-line/type type
                                             :command/name command)
                    is-ls-output? (hash-map :db/id ?input-line-eid
                                            :input-line/type type
                                            :output/file-name file-name
                                            :output/file-size file-size)))))
     (d/transact! conn))

;; Figure out which output belongs to each command
(->> (d/q '[:find ?output-eid
            :where
            [?output-eid :input-line/type :input-line/output]]
          @conn)
     (map (fn [[?output-eid]]
            (let [output (d/entity @conn ?output-eid)
                  command (->> output
                               (iterate #(d/entity @conn (:input-line/previous %)))
                               (take-while some?)
                               (filter #(= :input-line/command (:input-line/type %)))
                               (first))]
              (hash-map :db/id (:db/id output)
                        :output/for-command (:db/id command)))))
     (d/transact! conn))

;; Figure out each lines previous command
(->>
 (d/q '[:find ?input-line-eid ?previous-command-eid
        :where
        [?input-line-eid :input-line/previous ?previous-command-eid]
        [?previous-command-eid :input-line/type :input-line/command]]
      @conn)
 (map (fn [[?input-line-eid ?previous-command-eid]]
        (hash-map :db/id ?input-line-eid
                  :input-line/previous-command ?previous-command-eid)))
 (d/transact! conn))

(->>
 (repeatedly
  #(->> (d/q '[:find ?input-line-eid ?previous-command-eid
               :in $
               :where
               [?input-line-eid :input-line/previous ?previous-input-line-eid]
               [?previous-input-line-eid :input-line/previous-command ?previous-command-eid]
               [(missing? $ ?input-line-eid :input-line/previous-command)]]
             @conn)
        (map (fn [[?input-line-eid ?previous-command-eid]]
               (hash-map :db/id ?input-line-eid
                         :input-line/previous-command ?previous-command-eid)))
        (d/transact! conn)))
 (take 10))

;; Create directory entities for each directory visited
(->>
 (d/q '[:find ?directory-name
        :where
        [?command-eid :input-line/type :input-line/command]
        [?command-eid :command/arg ?directory-name]
        [(!= ?directory-name "..")]]
      @conn)
 (map (fn [[?directory-name]]
        (hash-map :directory/name ?directory-name)))
 (d/transact! conn))



(d/q '[:find ?e ?a ?v
       :where
       [?e ?a ?v]
       [(= 3 ?e)]]
     @conn)

(->> (d/q '[:find ?output-eid
            :where
            [?output-eid :input-line/type :input-line/output]]
          @conn)
     (map #(get (d/entity @conn (first %)) :output/for-command)))

(get (d/entity @conn 7) :output/for-command)

(map #(d/q '[:find ?a ?v :in $ ?e :where [?e ?a ?v]] @conn %) [21])

(->> (d/entity @conn 25)
     (iterate #(d/entity @conn (:input-line/previous %)))
     (take-while some?)
     (filter #(= :input-line/command (:input-line/type %)))
     (first))

(first (filter #(= :input-line/command (:input-line/type %))
               (take-while some? (iterate #(d/entity @conn (:input-line/previous %)) (d/entity @conn 5)))))

(take 23 (take-while some?
                     (iterate #(d/entity @conn (:input-line/previous %)) (d/entity @conn 5))))

(d/entity @conn (:input-line/previous (d/entity @conn 5)))

(->>
 (d/q '[:find ?previous-input-line-eid ?previous-input-line-number
        :where
        [?previous-input-line-eid :input-line/number ?previous-input-line-number]]
      @conn)
 (map (fn [[?previous-input-line-eid ?previous-input-line-number]]
        (let [next-input-line-number (inc ?previous-input-line-number)]
          ;; (d/q '[:find ?next-input-line-eid
          ;;        :where
          ;;        [?next-input-line-eid :input-line/number next-input-line-number]]
          ;;      @conn)
          next-input-line-number))))

(d/q '[:find ?next-input-line-eid ?previous-input-line-eid
       :where
       [?previous-input-line-eid :input-line/number 1]
       [?next-input-line-eid :input-line/number #(inc 1)]]
     @conn)

(->>
 (d/q '[:find ?input-line-entity-id ?input-line-text
        :where
        [?input-line-entity-id :input-line/text ?input-line-text]]
      @conn))

(map vector (repeat 1) (repeat :foobar) [10 20 30])



(d/transact! conn
             [{:user/id "1"
               :user/name "alice"
               :user/age 27}
              {:user/id "2"
               :user/name "bob"
               :user/age 29}
              {:user/id "3"
               :user/name "kim"
               :user/age 2
               :user/parent [[:user/id "1"]
                             [:user/id "2"]]}
              {:user/id "4"
               :user/name "aaron"
               :user/age 61}
              {:user/id "5"
               :user/name "john"
               :user/age 39
               :user/parent [[:user/id "4"]]}
              {:user/id "6"
               :user/name "mark"
               :user/age 34}
              {:user/id "7"
               :user/name "kris"
               :user/age 8
               :user/parent [[:user/id "4"]
                             [:user/id "5"]]}])

(d/q '[:find ?e ?n
       :where
       [?e :user/id]
       [?e :user/name ?n]]
     @conn)

(d/q '[:find [?n ...]
       :where
       [?e :user/id]
       [?e :user/name ?n]]
     @conn)

(d/q '[:find ?n .
       :where
       [?e :user/id]
       [?e :user/name ?n]]
     @conn)

(d/pull)
