(ns advent-of-code-2022.day-07.solution
  (:require [clojure.string :as str]))

;; Day 7 was a difficult one.
;; I decided to think of things from an entity and computed property
;; perspective, used functions to do the computed property part
;; (memoized them when necessary).
;; Really liked this approach because it was easy to test and allowed 
;; me to solve the problems in small incremental steps.
;; Also tried to solve it using DataScript but gave up once it 
;; got long and slow.

(def input-text (slurp "./src/advent_of_code_2022/day_07/input.txt"))
;; (def input-text (slurp "./src/advent_of_code_2022/day_07/sample.txt"))

;; Lines

;; Line numbers are required in order to correctly associate output lines
;; with command lines because some line texts are exactly the same
(def lines (vec (map list (range) (str/split-lines input-text))))

(defn line-number [line] (first line))
(defn line-text [line] (second line))

(defn line-previous [line] (nth lines (dec (line-number line)) nil))
(comment (map list lines (map line-previous lines)))

(defn line-type [line] (if (str/starts-with? (line-text line) "$") :command :output))
(comment (map list lines (map line-type lines)))

(defn line-is-command? [line] (= (line-type line) :command))
(defn line-is-output? [line] (= (line-type line) :output))

;; Commands

(def commands (filter line-is-command? lines))

(defn command-outputs [command]
  (take-while line-is-output? (drop (inc (line-number command)) lines)))
(comment (map list commands (map command-outputs commands)))

(defn command-type [command]
  (if (str/starts-with? (line-text command) "$ cd") :cd :ls))
(comment (map list commands (map command-type commands)))

(defn command-is-cd? [command] (= (command-type command) :cd))
(defn command-is-ls? [command] (= (command-type command) :ls))

;; Outputs

(def outputs (filter line-is-output? lines))

(defn output-command [output]
  (let [previous-line (line-previous output)]
    (if (line-is-command? previous-line)
      previous-line
      (output-command previous-line))))

(comment (map list outputs (map output-command outputs)))

;; cd

(def cd-commands (filter command-is-cd? commands))

(defn cd-command-args [command] (str/join (drop 5 (line-text command))))
(comment (map list cd-commands (map cd-command-args cd-commands)))

(defn line-wd [line]
  (if (and (line-is-command? line) (command-is-cd? line))
    (case (cd-command-args line)
      "/" ["/"]
      ".." (pop (line-wd (line-previous line)))
      (conj (line-wd (line-previous line)) (cd-command-args line)))
    (line-wd (line-previous line))))
(comment (map list lines (map line-wd lines)))

;; ls

(defn output-from-ls? [output] (command-is-ls? (output-command output)))

(def ls-outputs (filter output-from-ls? outputs))

(defn ls-output-type [ls-output]
  (if (str/starts-with? (line-text ls-output) "dir") :directory :file))
(comment (map list ls-outputs (map ls-output-type ls-outputs)))

(defn ls-output-is-file? [ls-output] (= (ls-output-type ls-output) :file))

(def ls-file-outputs (filter ls-output-is-file? ls-outputs))

(defn ls-file-output-size [ls-file-output]
  (read-string (first (str/split (line-text ls-file-output) #" "))))
(comment (map list ls-file-outputs (map ls-file-output-size ls-file-outputs)))

(defn ls-file-output-name [ls-file-output]
  (second (str/split (line-text ls-file-output) #" ")))
(comment (map list ls-file-outputs (map ls-file-output-name ls-file-outputs)))

;; Directories

(def directories (distinct (map line-wd lines)))

(def directory-subdirectories
  (memoize
   (fn [directory]
     (filter #(cond (= directory %) false
                    (not= (inc (count directory)) (count %)) false
                    :else (every? true? (map = directory %))) directories))))
(comment (map list directories (map directory-subdirectories directories)))

(def directory-files
  (memoize (fn [directory] (filter #(= directory (line-wd %)) ls-file-outputs))))
(comment (map list directories (map directory-files directories)))

(def directory-size
  (memoize (fn [directory]
             (let [size-of-files-in-directory (reduce + 0 (map ls-file-output-size (directory-files directory)))
                   size-of-subdirectories (reduce + 0 (map directory-size (directory-subdirectories directory)))]
               (+ size-of-files-in-directory size-of-subdirectories)))))
(comment (map list directories (map directory-size directories)))

;; Answer 1: 1886043
(->> directories
     (map directory-size)
     (filter #(< % 100000))
     (reduce +))

;; Answer 2: 3842121
(def disk-total 70000000)
(def disk-used (directory-size ["/"]))
(def disk-remaining (- disk-total disk-used))

(def update-size 30000000)

(def space-required-to-delete (- update-size disk-remaining))

(->> directories
     (map directory-size)
     (filter #(> % space-required-to-delete))
     (apply min))
