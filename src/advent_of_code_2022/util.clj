(ns advent-of-code-2022.util)

(defn all-chars [str]
  (vec (re-seq #"[A-Za-z]+" str)))

(defn all-numbers [str]
  (mapv parse-long (re-seq #"[1-9]+" str)))

(defn first-index-of [p xs]
  (->> xs
    (keep-indexed (fn [i x] (when (p x) i)))
    first))


