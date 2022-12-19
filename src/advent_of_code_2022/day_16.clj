(ns advent-of-code-2022.day_16
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "resources/day_16_input.txt"))

(defn all-chars [input]
  (->> input
    (s/split-lines)
    (map u/all-chars)))

(defn flow-rates [input]
  (->> input
    (s/split-lines)
    (map u/all-numbers)))


(apply hash-map (interleave [:a :b :c] (repeat 1)))

(defn tunnel-graph [lines]
  (->> lines
    (map (fn [xs] [(get xs 1) (subvec xs 9 (count xs))]))
    (map (fn [[from to]] (hash-map from (apply hash-map (interleave to (repeat 1))))))
    (reduce into)))

(defn find-shortest-paths [graph]
  (reduce into (for [from (keys graph)
                     to (keys graph)]
                 {(str from to) (get (u/dijkstra graph from to) to)})))

;(get shortest-paths (str "AA" (first nil)))
;(get valves-with-flow nil)
;valves-with-flow

(defn max-release [permutation shortest-paths valves]
  (let [init-walk (get shortest-paths (str "AA" (first permutation)))]
    (loop [[p & ps :as all] permutation
           move 0
           time (+ init-walk 1)
           release 0
           sum-release 0]
      ;(println "--------------")
      ;(println "minute" time)
      ;(println p "& " ps)
      ;(println "move" move)
      ;(println "release" release)
      ;(println "sum-release" sum-release)
      (if (> time 30)
        sum-release
        (if (= 0 move)
          (recur
            ps
            (or (get shortest-paths (str p (first ps))) 0)
            (inc time)
            (+ release (or (get valves p) 0))
            (+ sum-release release))
          (recur
            all
            (dec move)
            (inc time)
            release
            (+ sum-release release)))))))




;(defn shortest-paths)
(def lines (all-chars input))
(def flows (flow-rates input))
(def valves (map #(get % 1) lines))
(def valves-with-flow (reduce into (filter some? (map (fn [v [f]] (when (> f 0) {v f})) valves flows))))
(def graph (tunnel-graph lines))
(def shortest-paths (find-shortest-paths graph))

(Throwable->map *e)

#_(def permutation (reverse (map first (sort-by second (map (fn [[v flow]]
                                                              (if (not= v "AA")
                                                                [v (/ flow (get shortest-paths (str "AA" v)))]
                                                                [v 0])) valves-with-flow)))))


valves-with-flow
shortest-paths

(defn get-candidates [visited valves-with-flow]
  (filter (fn [[v _]] (not (get visited v))) valves-with-flow))

;(get-candidates #{"AA" "CC" "BB" "DD"} valves-with-flow)
;(into #{"AA" "VV"} ["EE"])

(defn branch&bound [valves-with-flow shortest-paths]
  (loop [[branch & bs] [{:path ["AA"]
                         :visited #{"AA"}
                         :minute 1}]
         finished []
         iter 0]
    ;(println "loop with ")
    ;(println "branch " branch)
    ;(println "finished " finished)
    ;(println "iter " iter)
    (when (= (mod iter 100000) 0) (println "iteration" (/ iter 1000) "k"))
    (if (nil? branch)
      finished
      (if (or (> (:minute branch) 30) (= (count (:path branch)) (inc (count valves-with-flow))))
        (recur bs (conj finished (:path branch)) (inc iter))
        (let [candidates (get-candidates (:visited branch) valves-with-flow)
              new-paths (reduce (fn [paths [v _]]
                                  (conj paths {:path (conj (:path branch) v)
                                               :visited (into (:visited branch) [v])
                                               :minute (+
                                                         (:minute branch)
                                                         (get shortest-paths (str (peek (:path branch)) v))
                                                         1)}))
                          []
                          candidates)]
          (recur (into bs new-paths) finished (inc iter)))))))


;(peek ["AA" "BB" "CC"])
;(conj ["AA" "BB" "CC"] "UU")
(def branches (branch&bound valves-with-flow shortest-paths))
branches
(println "max release" (apply max (map #(max-release (rest %) shortest-paths valves-with-flow) branches)))

;(max-release  [ "DD" "BB" "JJ" "HH" "EE" "CC"] shortest-paths valves-with-flow)

;(count branches)
;valves-with-flow
;shortest-paths



#_(println (max-release permutation shortest-paths valves-with-flow))

(comment
  lines

  flows
  valves
  valves-with-flow


  graph
  (get valves-with-flow "JJ")
  (keys graph)
  (get (u/dijkstra graph "DD" "JJ") "JJ")
  (count (combo/permutations (keys valves-with-flow)))


  (sort-by val valves-with-flow)

  permutation

  (get-next-best-order shortest-paths valves-with-flow)

  '("JJ" "DD" "HH" "BB" "EE" "CC")
  (def best '("DD" "BB" "JJ" "HH" "EE" "CC"))
  (max-release '("JJ" "DD" "HH" "BB" "EE" "CC") shortest-paths valves-with-flow)
  (max-release best shortest-paths valves-with-flow)

  )
