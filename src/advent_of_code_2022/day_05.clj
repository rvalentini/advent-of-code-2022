(ns advent-of-code-2022.day_05
  (:require [clojure.java.io :as io]))

(def n-stacks 9)
(def command #".*?(\d+).*(\d+).*(\d+)")

(defn pos->cargo [line pos]
  (when
    (and
      (> (.length line) (inc pos))
      (= \[ (nth line (dec pos))))
    (subs line pos (inc pos))))

(defn line->cargo [l]
  (let [indices (map #(+ (* (dec %) 4) 1) (range 1 (inc n-stacks)))]
    (->> indices (map (partial pos->cargo l)))))

(defn line->command [l]
  (map parse-long (drop 1 (re-find command l))))

(defn stack-cargo [cargo]
  (let [init (into {} (map #(identity {% '()}) (range 1 (inc n-stacks))))
        stack (reduce (fn [stack level]
                        (reduce
                          (fn [stack [i c]] (update stack i #(conj % c)))
                          stack
                          (map-indexed #(identity [(inc %1) %2]) level)))
                init
                cargo)]
    (update-vals stack #(->> % (filter some?) reverse))))

(defn apply-command-9000 [stack cmd]
  (let [[n from to] cmd]
    (last (take (inc n) (iterate (fn [stack]
                                   (as-> stack s
                                     (update s to #(conj % (first (get s from))))
                                     (update s from rest))) stack)))))

(defn apply-command-9001 [stack cmd]
  (let [[n from to] cmd]
    (as-> stack s
      (update s to #(concat (take n (get s from)) %))
      (update s from #(drop n %)))))

(defn process-cargo [cargo cmds apply-fn]
  (reduce (fn [cargo cmd] (apply-fn cargo cmd)) cargo cmds))

(comment
  (with-open [rdr (io/reader (io/resource "day_05_input.txt"))]
    (let [input (->> (line-seq rdr) (partition-by #(= "" %)))
          cargo (->> input
                  first
                  drop-last
                  (map line->cargo)
                  stack-cargo)
          commands (->> input
                     last
                     (map line->command))
          final-cargo-9000 (process-cargo cargo commands apply-command-9000)
          final-cargo-9001 (process-cargo cargo commands apply-command-9001)
          get-tops #(reduce str (map first (vals (sort-by key (doall %)))))]

      (println "Final top cargo for 9000 crane: " (get-tops final-cargo-9000))
      (println "Final top cargo for 9001 crane: " (get-tops final-cargo-9001))))

  )
