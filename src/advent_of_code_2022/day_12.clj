(ns advent-of-code-2022.day_12
  (:require [clojure.string :as s]))

(def input (slurp "resources/day_12_input.txt"))

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (mapv #(s/split % #""))))

(defn allowed-move? [current next]
  (let [current (if (= current "S") "a" current)
        next (if (= next "E") "z" next)]
    (<= (int (.charAt next 0)) (inc (int (.charAt current 0))))))

(defn within-bounds? [[x y] grid]
  (if (or (< x 0) (< y 0) (>= y (count grid)) (>= x (count (first grid))))
    nil
    [x y]))

(defn get-cell [grid [x y]]
  (get-in grid [y x]))

(defn assoc-cell [grid [x y] val]
  (assoc-in grid [y x] val))

(defn get-candidates [[x y] grid seen]
  (->> [[x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y]]
    (keep #(within-bounds? % grid))
    (keep #(when (not (get seen %)) %))
    (keep #(when (allowed-move? (get-cell grid [x y]) (get-cell grid %)) %))))

(defn all-cells [grid]
  (for [i (range 0 (count grid))
        j (range 0 (count (first grid)))]
    [(get-in grid [i j]) [j i]]))

(defn get-start [grid]
  (->> (all-cells grid)
    (keep (fn [[level pos]] (when (= level "S") pos)))
    first))

(defn get-all-as [grid]
  (->> (all-cells grid)
    (keep (fn [[level pos]] (when (or (= level "S") (= level "a")) pos)))))

(defn shortest-path [grid dist-grid start]
  (loop [dist-grid dist-grid
         current-dist 0
         [current & rest] [start]
         seen #{}]
    (if (some? current)
      (if (= (get-cell grid current) "E")
        {:dist-grid dist-grid :end current}
        (let [neighbors (get-candidates current grid seen)
              updated-dist-grid
              (reduce
                (fn [dist-grid n]
                  (let [{:keys [dist]} (get-cell dist-grid n)
                        new-dist (inc current-dist)]
                    (if (< new-dist dist)
                      (assoc-cell dist-grid n {:pred current :dist new-dist})
                      dist-grid)))
                dist-grid neighbors)]
          (recur
            updated-dist-grid
            (inc current-dist)
            (concat (vec rest) (vec neighbors))
            (into seen (conj neighbors current)))))
      :no-path)))

(defn build-dist-grid [grid]
  (vec (for [_ (range 0 (count grid))]
         (vec (for [_ (range 0 (count (first grid)))]
                {:dist Integer/MAX_VALUE
                 :prev nil})))))

(defn backtrack-path [{:keys [dist-grid end]} start]
  (loop [curr end
         path []]
    (if (= curr start)
      path
      (recur
        (:pred (get-cell dist-grid curr))
        (conj path curr)))))

(defn find-shortest-path-length [grid dist-grid start]
  (let [result (shortest-path grid dist-grid start)]
    (if (not= result :no-path)
      (count (backtrack-path result start))
      Integer/MAX_VALUE)))

(defn part1 [grid dist-grid]
  (find-shortest-path-length grid dist-grid (get-start grid)))

(defn part2 [grid dist-grid start-points]
  (apply min (for [start start-points]
               (find-shortest-path-length grid dist-grid start))))

(let [grid (parse-input input)
      dist-grid (build-dist-grid grid)]
  (assert (= (part1 grid dist-grid) 497))
  (assert (= (part2 grid dist-grid (get-all-as grid)) 492)))
