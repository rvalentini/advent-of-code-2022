(ns advent-of-code-2022.day_03
  (:require [clojure.java.io :as io]))

(defn find-duplicate [cols]
  (first (keys (filter
                 (fn [[_ v]] (> v (- (count cols) 1)))
                 (frequencies (apply concat (map set cols)))))))

(defn char->priority [c]
  (let [ascii (int c)]
    (- ascii (if (< ascii 96) 38 96))))

(comment
  (with-open [rdr (io/reader (io/resource "day_03_input.txt"))]
    (let [rucksacks (->> (line-seq rdr))]

      (println "Sum of all priorities: "
        (->> rucksacks
          (map seq)
          (map #(split-at (/ (count %) 2) %))
          (map find-duplicate)
          (map char->priority)
          (reduce +)))

      (println "Sum of all group priorities: "
        (->> rucksacks
          (map seq)
          (partition 3)
          (map find-duplicate)
          (map char->priority)
          (reduce +)))))
  )
