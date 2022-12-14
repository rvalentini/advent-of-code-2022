(ns advent-of-code-2022.day_14
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def input (slurp "resources/day_14_input.txt"))

(def sand-source [500 0])
(def x-start 100)
(def x-end 900)

(defn range->single [[[sx sy] [ex ey]]]
  (if (= sx ex)
    (for [y (range (min sy ey) (inc (max sy ey)))] [sx y])
    (for [x (range (min sx ex) (inc (max sx ex)))] [x sy])))

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (map u/all-numbers)
    (map #(partition 2 %))
    (map #(partition 2 1 %))
    (map #(map range->single %))
    (map #(apply concat %))
    (apply concat)))

(defn add-bottom [rocks]
  (concat rocks (let [max-y (apply max (map second rocks))]
                  (for [x (range x-start x-end)]
                    [x (+ 2 max-y)]))))

(defn get-pos [cave-map [x y]]
  (get-in cave-map [y (- x x-start)]))

(defn move-sand-to [cave-map [old-x old-y] [new-x new-y]]
  (-> cave-map
    (u/mark-at-pos "." [(- old-x x-start) old-y])
    (u/mark-at-pos "o" [(- new-x x-start) new-y])))

(defn blocked? [cave-map pos]
  (not= (get-pos cave-map pos) "."))

(defn move-sand [cave-map]
  (loop [[x y :as pos] sand-source
         cave-map (u/mark-at-pos cave-map "o" [(- x x-start) y])]
    (let [down [x (inc y)]
          down-left [(dec x) (inc y)]
          down-right [(inc x) (inc y)]]
      (if (not (blocked? cave-map down))
        (recur down (move-sand-to cave-map pos down))
        (if (not (blocked? cave-map down-left))
          (recur down-left (move-sand-to cave-map pos down-left))
          (if (not (blocked? cave-map down-right))
            (recur down-right (move-sand-to cave-map pos down-right))
            cave-map))))))

(defn into-abyss? [cave-map]
  (some #{"o"} (get cave-map 179)))

(defn source-blocked? [cave-map]
  (= "o" (get-pos cave-map [500 0])))

(defn part1 [cave-map]
  (->> cave-map
    (iterate move-sand)
    (take-while #(not (into-abyss? %)))
    count
    dec))

(defn part2 [cave-map]
  (->> cave-map
    (iterate move-sand)
    (take-while #(not (source-blocked? %)))
    count))

(let [rocks (add-bottom (parse-input input))
      matrix (u/build-matrix {:x-dim (- x-end x-start)
                              :y-dim 190
                              :init-fn (constantly ".")})
      cave-map (reduce
                 (fn [m [x y]] (u/mark-at-pos m "#" [(- x x-start) y]))
                 matrix
                 rocks)]
  (assert (= (part1 cave-map) 913))
  (assert (= (part2 cave-map) 30762)))

