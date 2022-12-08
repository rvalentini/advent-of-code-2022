(ns advent-of-code-2022.day_08
  (:require [clojure.string :as s]))

(def input (slurp "resources/day_08_input.txt"))

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (map #(s/split % #""))
    (map #(mapv parse-long %))
    vec))

(defn is-visible-row [row x]
  (let [height (get row x)]
    [(every? #(< % height) (subvec row 0 x))
     (every? #(< % height) (subvec row (inc x) (count row)))]))

(defn is-visible [grid [x y]]
  (let [transposed (apply mapv vector grid)
        [above below] (is-visible-row (get transposed x) y)
        [left right] (is-visible-row (get grid y) x)]
    [above below left right]))

(defn inner-trees [grid]
  (for [x (range 1 (dec (count grid)))
        y (range 1 (dec (count (first grid))))]
    [x y]))

(defn visible-interior [grid]
  (->> (inner-trees grid)
    (map #(is-visible grid %))
    (keep #(some true? %))
    count))

(defn visible-exterior [grid]
  (- (* 2 (+ (count (first grid)) (count grid))) 4))

(defn view-score [height coll]
  (let [taken (count (take-while #(< % height) coll))
        remaining (drop-while #(< % height) coll)]
    (if (not-empty remaining) (inc taken) taken)))

(defn scenic-score-row [row x]
  (let [height (get row x)]
    [(view-score height (reverse (subvec row 0 x)))
     (view-score height (subvec row (inc x) (count row)))]))

(defn scenic-score [grid [x y]]
  (let [transposed (apply mapv vector grid)
        [above below] (scenic-score-row (get transposed x) y)
        [left right] (scenic-score-row (get grid y) x)]
    [above left below right]))

(defn scenic-scores [grid]
  (for [x (range 0 (count grid))
        y (range 0 (count (first grid)))]
    (scenic-score grid [x y])))

(defn part1 [input]
  (let [grid (parse-input input)]
    (+ (visible-interior grid) (visible-exterior grid))))

(defn part2 [input]
  (->> (parse-input input)
    scenic-scores
    (map #(apply * %))
    (apply max)))

(comment
  ;1733
  (part1 input)
  ; 284648
  (part2 input)
  )
