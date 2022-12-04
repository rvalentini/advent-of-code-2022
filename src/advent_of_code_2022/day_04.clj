(ns advent-of-code-2022.day_04
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn overlap? [a b a&b] (< (count a&b) (+ (count a) (count b))))
(defn contained? [a b a&b] (or (= (count a&b) (count a)) (= (count a&b) (count b))))

(defn assignment->sections [a]
  (let [[low hi] (map parse-long (s/split a #"-"))]
    (range low (inc hi))))

(defn compare-assignments [[ass1 ass2] comp-fn]
  (let [a (assignment->sections ass1)
        b (assignment->sections ass2)
        a&b (into #{} (concat a b))]
    (comp-fn a b a&b)))

(comment
  (with-open [rdr (io/reader (io/resource "day_04_input.txt"))]
    (let [pairs (->> (line-seq rdr))]

      (println "Count of fully contained sections: "
        (->> pairs
          (map #(s/split % #","))
          (map #(compare-assignments % contained?))
          (filter identity)
          count))

      (println "Count of overlapping sections: "
        (->> pairs
          (map #(s/split % #","))
          (map #(compare-assignments % overlap?))
          (filter identity)
          count))))
  )
