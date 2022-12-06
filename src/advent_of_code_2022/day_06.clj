(ns advent-of-code-2022.day_06)

(def input (slurp "resources/day_06_input.txt"))

(def packet-length 4)
(def message-length 14)

(defn first-marker-pos [l]
  (some
    (fn [[i p]] (when (apply distinct? p) (+ l i)))
    (map-indexed vector (partition l 1 input))))

(comment
  (first-marker-pos packet-length)
  (first-marker-pos message-length)

  )
