(ns advent-of-code-2022.day_16
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "resources/day_16_input.txt"))

(defn line-chars [input]
  (->> input
    (s/split-lines)
    (map u/all-chars)))

(defn flow-rates [input]
  (->> input
    (s/split-lines)
    (map u/all-numbers)))

(defn tunnel-graph [lines]
  (->> lines
    (map (fn [xs] [(get xs 1) (subvec xs 9 (count xs))]))
    (map (fn [[from to]] (hash-map from (apply hash-map (interleave to (repeat 1))))))
    (reduce into)))

(defn find-shortest-paths [graph]
  (reduce into (for [from (keys graph)
                     to (keys graph)]
                 {(str from to) (get (u/dijkstra graph from to) to)})))

(defn max-release [path shortest-paths valves time]
  (let [init-walk (get shortest-paths (str "AA" (first path)))]
    (loop [[p & ps :as all] path
           move 0
           time-passed (+ init-walk 1)
           release 0
           sum-release 0]
      (if (> time-passed time)
        sum-release
        (if (zero? move)
          (recur
            ps
            (or (get shortest-paths (str p (first ps))) 0)
            (inc time-passed)
            (+ release (or (get valves p) 0))
            (+ sum-release release))
          (recur
            all
            (dec move)
            (inc time-passed)
            release
            (+ sum-release release)))))))

(defn extend-path [branch valve shortest-paths]
  {:path (conj (:path branch) valve)
   :visited (into (:visited branch) [valve])
   :minute (+
             (:minute branch)
             (get shortest-paths (str (peek (:path branch)) valve))
             1)})

(defn branch&bound [valves-with-flow shortest-paths]
  (let [time-limit-reached? #(> (:minute %) 26)
        all-valves-open? #(= (count (:path %)) (inc (count valves-with-flow)))
        get-candidates #(filter (fn [[v _]] (not (% v))) valves-with-flow)]
    (loop [[branch & bs] [{:path ["AA"] :visited #{"AA"} :minute 1}]
           finished []]
      (if (nil? branch)
        finished
        (if (or (time-limit-reached? branch) (all-valves-open? branch))
          (recur bs (conj finished (:path branch)))
          (let [candidates (get-candidates (:visited branch))
                new-paths (reduce
                            (fn [paths [v _]] (conj paths (extend-path branch v shortest-paths)))
                            []
                            candidates)]
            (recur (into bs new-paths) finished)))))))

(defn valves->flow [chars numbers]
  (->> [chars (flatten numbers)]
    (apply map vector)
    (filter (comp pos? second))
    (into {})))

(defn part1 [shortest-paths valves-with-flow]
  (let [branches (branch&bound valves-with-flow shortest-paths)]
    (->> branches
      (map #(max-release (rest %) shortest-paths valves-with-flow 30))
      (apply max))))

(defn part2 [shortest-paths valves-with-flow]
  (let [balanced-subsets (fn [xs] (every? #(> (count %) 6) xs))
        branches-with-elephant (->> (combo/partitions valves-with-flow {:min 2 :max 2})
                                 (filter balanced-subsets)
                                 (map (fn [xs] (map #(branch&bound % shortest-paths) xs))))]
    (->> branches-with-elephant
      (map (fn [paths-with-elephant]
             (map (fn [paths]
                    (->> paths
                      (map #(max-release (rest %) shortest-paths valves-with-flow 26))
                      (apply max)))
               paths-with-elephant)))
      (map (fn [[r1 r2]] (+ r1 r2)))
      (apply max))))

(let [lines (line-chars input)
      valves (map #(get % 1) lines)
      flows (flow-rates input)
      shortest-paths (->> lines
                       tunnel-graph
                       find-shortest-paths)
      valves-with-flow (valves->flow valves flows)]
  (assert (= (part1 shortest-paths valves-with-flow) 2056))
  (assert (= (part2 shortest-paths valves-with-flow) 2513)))
