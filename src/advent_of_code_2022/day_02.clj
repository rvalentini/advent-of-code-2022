(ns advent-of-code-2022.day_02
  (:require [clojure.java.io :as io]))

(def round->points
  {"A X" 4
   "A Y" 8
   "A Z" 3
   "B X" 1
   "B Y" 5
   "B Z" 9
   "C X" 7
   "C Y" 2
   "C Z" 6})

(def result->round
  {"A X" "A Z"
   "A Y" "A X"
   "A Z" "A Y"
   "B X" "B X"
   "B Y" "B Y"
   "B Z" "B Z"
   "C X" "C Y"
   "C Y" "C Z"
   "C Z" "C X"})

(comment
  (with-open [rdr (io/reader (io/resource "day_02_input.txt"))]
    (let [rounds (->> (line-seq rdr))]

      (println "Strategy 1 score: "
        (->> rounds
          (map #(get round->points %))
          (reduce +)))

      (println "Strategy 2 score: "
        (->> rounds
          (map #(get result->round %)  )
          (map #(get round->points %))
          (reduce +)))))
  )






