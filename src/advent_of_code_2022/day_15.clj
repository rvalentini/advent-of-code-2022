(ns advent-of-code-2022.day_15
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def input (slurp "resources/day_15_input.txt"))

(defn parse-input [input]
  (s/split-lines input))

(comment
  (def lines (parse-input input))
  lines

  )
