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

; try with inc only
;(defn max-release [permutation shortest-paths valves]
;  (let [init-walk (get shortest-paths (str "AA" (first permutation)))]
;    (loop [[p & ps] permutation
;           release 0
;           release-sum 0
;           time (inc init-walk)]
;      (println "----------------------")
;      (println "minute" time)
;      (println "loop with " p "and" ps)
;      (println "current release each minute " release)
;      (println "total release" release-sum )
;      (if (> time 30)
;        release-sum
;        (let [new-release (+ release (if (some? p)
;                                       (get valves p)
;                                       0))      ;existing + (existing +  new one) * steps
;              time-passed (+ 1
;                            (if (some? ps)
;                              (get shortest-paths (str p (first ps)))
;                              0))
;              time-remaining (- 30 time)
;              new-total-release (+ release-sum release (if (some? ps)
;                                                         (*
;                                                           (Math/min (get shortest-paths (str p (first ps))) time-remaining)
;                                                           new-release)
;                                                         0))]                          ; + 1  because of open
;          (println "new release" new-release)
;          (println "new total release" new-total-release)
;          (println "time passed" time-passed)
;          (println "time remaining" (- 30 time))
;          (recur
;            ps
;            new-release
;            (+ (if (< 30 (+ time time-passed))
;                 (* new-release (- 30 time))
;                 0) new-total-release)
;            (+ time time-passed)))))))


;(defn get-next-best-order [shortest-paths valves-with-flow]
;  (loop [pos "AA"
;         path []
;         remaining-valves (set (keys valves-with-flow))
;         time-remaining 30]
;    (println "--------------")
;    (println "loop with pos" pos)
;    (println "loop with path" path)
;    (println "loop with remaining" remaining-valves)
;    (println "loop with time rem" time-remaining)
;    (if (and (not-empty remaining-valves) (> time-remaining 0))
;      (let [[next _ new-time-remaining] (reduce (fn [[next max-gain max-time :as old] v]
;                                                  (let [travel-time (get shortest-paths (str pos v))
;                                                        curr-time (- time-remaining (inc travel-time))
;                                                        gain (* curr-time (get valves-with-flow v))
;                                                        delta_flow (Math/abs (- (get shortest-paths (str pos v))
;                                                                               (get shortest-paths (str pos next))))
;                                                        delta-steps (Math/abs (- max-time curr-time))]
;                                                    (println "curr " v ": time" curr-time "gain" gain)
;
;                                                    (if (and (> gain max-gain) (<) (* delta-steps delta_flow))
;                                                      [v gain curr-time]
;                                                      old)))
;                                          [pos 0 0]
;                                          remaining-valves)]
;        (recur next
;          (conj path next)
;          (remove #(= % next) remaining-valves)
;          new-time-remaining)
;        )
;      path)
;    )
;  )

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

#_permutation

(def variable (keys (take 13 (reverse (sort-by second valves-with-flow)))))
(def fixed '("NJ" "LL"))

(def input (map (fn [p] (concat p fixed)) (combo/permutations variable)))


variable
fixed



(println (reduce (fn [max [idx p]]
                   ;(println "called " idx "and" p)
                   (when (= (mod idx 10000000) 0) (println "Iteration" idx "with max" max))
                   (let [curr (max-release p shortest-paths valves-with-flow)]
                     (if (< max curr) curr max)))
           0
           (map-indexed vector input)))

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
