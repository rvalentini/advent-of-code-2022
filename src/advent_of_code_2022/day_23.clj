(ns advent-of-code-2022.day_23
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def directions (cycle [:north :south :west :east]))

(def input (slurp "resources/day_23_input.txt"))

(defn is-elf? [matrix pos]
  (= (u/get-pos matrix pos) "#"))

(defn elves-in-vicinity [matrix pos]
  (seq (filter (partial is-elf? matrix) (u/neighbors-with-corners pos))))

(defn cardinal->dir [dir]
  (case dir
    :north :up
    :south :down
    :west :left
    :east :right
    :unknown))

(defn dir->edges [dir]
  (if (some #{dir} '(:up :down))
    '(:left :right)
    '(:up :down)))

(defn direction-valid? [matrix pos {:keys [desired edges]}]
  (when-let [neighbors (elves-in-vicinity matrix pos)]
    (when-not (some (set (conj edges desired)) neighbors)
      desired)))

(defn round->directions [n]
  (take-last 4 (take (+ 3 n) directions)))

(defn desired-pos [elf dir]
  (let [new-dir (cardinal->dir dir)
        desired (u/move elf new-dir)]
    {:desired desired
     :edges (map #(u/move desired %) (dir->edges new-dir))}))

(defn make-proposal [matrix elf directions]
  (->> directions
    (map #(desired-pos elf %))
    (filter #(not (u/outside-matrix? (:desired %) matrix)))
    (filter #(direction-valid? matrix elf %))
    first
    :desired))

(defn elf-positions [matrix]
  (for [y (range 0 (count matrix))
        x (range 0 (count (first matrix)))
        :when (is-elf? matrix [x y])]
    [x y]))

(defn proposals [matrix directions]
  (reduce
    (fn [acc elf]
      (if-let [proposed-next (make-proposal matrix elf directions)]
        (if (some? (acc proposed-next))
          (dissoc acc proposed-next)
          (assoc acc proposed-next elf))
        acc))
    {}
    (elf-positions matrix)))

(defn move [matrix proposals]
  (reduce
    (fn [m [to from]]
      (-> m
        (u/mark-at-pos "." from)
        (u/mark-at-pos "#" to)))
    matrix
    proposals))

(defn run-round [[matrix n]]
  (let [directions (round->directions n)
        proposals (proposals matrix directions)]
    [(move matrix proposals)
     (inc n)]))

(defn all-empty? [xs] (every? #(= "." %) xs))

(defn trim [matrix]
  (->> matrix
    (drop-while all-empty?)
    reverse
    (drop-while all-empty?)
    u/transpose
    (drop-while all-empty?)
    reverse
    (drop-while all-empty?)))

(defn sum-empty [matrix]
  (->> matrix
    flatten
    (filter #(= % "."))
    count))

(defn parse-input [input]
  (-> input
    s/split-lines
    u/lines->matrix))

(defn part1 [matrix]
  (-> (take 11 (iterate run-round [matrix 1]))
    last
    first
    trim
    sum-empty))

(defn part-2 [matrix]
  (loop [[current round] [matrix 1]]
    (let [[next next-round] (run-round [current round])]
      (if (= next current)
        round
        (recur [next next-round])))))

(let [matrix (u/expand (parse-input input) 300)]
  (println "Number of empty ground tiles:" (part1 matrix))
  (println "First round without movement:" (part-2 matrix)))
