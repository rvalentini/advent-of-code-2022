(ns advent-of-code-2022.day_01
  (:require [clojure.java.io :as io]))

(comment
  (with-open [rdr (io/reader (io/resource "day_01_input.txt"))]
    (let [calories (->> (line-seq rdr)
                  (partition-by #(= % ""))
                  (take-nth 2)
                  (map #(map parse-long %))
                  (map #(reduce + %)))]
      (println "Top elf: " (apply max calories))
      (println "Top three elves: " (apply + (take 3 (reverse (sort calories)))))))

  )

