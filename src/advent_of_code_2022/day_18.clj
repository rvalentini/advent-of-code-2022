(ns advent-of-code-2022.day_18
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def input (slurp "resources/day_18_input.txt"))

(defn cube->sides [[x y z]]
  [[[x y z] [(inc x) y z]]
   [[(dec x) y z] [x y z]]
   [[x y z] [x (inc y) z]]
   [[x (dec y) z] [x y z]]
   [[x y z] [x y (inc z)]]
   [[x y (dec z)] [x y z]]])

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (map u/all-numbers)))

(defn connected-sides [sides]
  (loop [[s & ss] sides
         seen #{}
         double #{}]
    (if (nil? s)
      double
      (if (get seen s)
        (recur ss seen (conj double s))
        (recur ss (conj seen s) double)))))

(defn surface-sides [sides]
  (loop [[s & ss] sides
         seen #{}
         unique #{}]
    (if (nil? s)
      unique
      (if (get seen s)
        (recur ss seen (set (remove #(= % s) unique)))
        (recur ss (conj seen s) (conj unique s))))))

(defn connected-air-sides [side surface-sides connected]
  (let [[c1 c2] side
        adjacent-sides (filter #(not= % side) (concat (cube->sides c1) (cube->sides c2)))
        inside-or-surface? #(or (get connected %) (get surface-sides %))]
    (filter #(not (inside-or-surface? %)) adjacent-sides)))

(defn reached-the-ocean? [side]
  (some (some-fn #(neg? %) #(> % 21)) (flatten side)))

(defn fresh? [side visited seen] (not (or (get visited side) (get seen side))))

(defn explore [side surface-sides connected]
  (loop [[s & rest] [side]
         seen #{}
         visited #{}]
    (if (nil? s)
      :closed
      (if (reached-the-ocean? s)
        :open
        (let [air-sides (connected-air-sides s surface-sides connected)
              fresh-air-sides (filter #(fresh? % visited seen) air-sides)]
          (recur
            (concat rest fresh-air-sides)
            (into seen fresh-air-sides)
            (conj visited s)))))))

(defn part1 [sides]
  (count (surface-sides sides)))

(defn part2 [sides]
  (let [surface-sides (set (surface-sides sides))
        internal-sides (set (connected-sides sides))
        internal-surface-area (->> surface-sides
                                (map #(explore % surface-sides internal-sides))
                                (filter #(= % :closed))
                                count)]
    (- (count surface-sides) internal-surface-area)))


(let [sides (apply concat (map cube->sides (parse-input input)))]
  (assert (= (part1 sides) 4282))
  (assert (= (part2 sides) 2452)))
