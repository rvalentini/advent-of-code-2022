(ns advent-of-code-2022.day_13
  (:require [clojure.string :as s]
            [clojure.edn :as  edn]))

(def input (slurp "resources/day_13_input.txt"))

(def separators [[[2]] [[6]]])

(defn find-first [p coll]
  (first (drop-while #(not (p %)) coll)))

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (partition 3 3 "pad")
    (map butlast)
    (map (fn [p] (map edn/read-string p)))))

(defn in-order? [l r]
  (cond
    (and (vector? l) (vector? r))
    (if-let [order (find-first (comp not zero?) (map in-order? l r))]
      order
      (compare (count l) (count r)))
    (and (vector? l) (not (vector? r))) (recur l [r])
    (and (not (vector? l)) (vector? r)) (recur [l] r)
    :else (compare l r)))

(defn part1 [lines]
  (->> lines
    (map #(apply in-order? %))
    (keep-indexed
      (fn [idx n] (when (or (= n -1) (= n 0)) (inc idx))))
    (apply +)))

(defn part2 [lines]
  (let [packages (concat separators (apply concat lines))
        sorted (sort-by identity in-order? packages)]
    (->> sorted
      (keep-indexed (fn [i p] (when (some #{p} separators) (inc i))))
      (apply *))))


(assert (= 5555 (part1 (parse-input input))))
(assert (= 22852 (part2 (parse-input input))))
