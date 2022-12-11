(ns advent-of-code-2022.util)

(defn all-chars [str]
  (vec (re-seq #"[A-Za-z]+" str)))

(defn all-numbers [str]
  (mapv parse-long (re-seq #"[1-9]+" str)))

(defn first-index-of [p xs]
  (->> xs
    (keep-indexed (fn [i x] (when (p x) i)))
    first))


(defn mark-with-at-pos [matrix i [x y]]
  (assoc-in matrix [y x] i))

(defn print-rope [rope]
  (let [matrix (vec (map vec (for [x (range 0 21)]
                               (for [y (range 0 26)]
                                 "."))))
        head (first rope)
        tail (peek rope)
        middle (map-indexed (fn [i p] ["#" p]) (butlast (rest rope)))]
    (doseq [l (reduce
                (fn [matrix [i pos]] (mark-with-at-pos matrix i pos))
                (-> matrix
                  (mark-with-at-pos "H" head)
                  (mark-with-at-pos "T" tail))
                middle)]
      (println l))
    ))
