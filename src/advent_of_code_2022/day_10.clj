(ns advent-of-code-2022.day_10
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def input (slurp "resources/day_10_input.txt"))

(defn parse-command [c]
  (case (.substring c 0 4)
    "noop" 0
    "addx" (first (u/all-numbers c))
    :else :error))

(defn parse-input [input]
  (->> input
    s/split-lines
    (map parse-command)))

(def output-idices [20 60 100 140 180 220])

(defn binc [i] (inc(inc i)))

(defn program->cycles [program]
  (reduce (fn [{:keys [now register] :as cycles} cmd]
            (if (= cmd 0)
              (-> cycles
                (assoc now register)
                (update :now inc))
              (-> cycles
                (assoc now register)
                (assoc (inc now) (+ register cmd))
                (update :now binc)
                (update :register (partial + cmd)))))
    {:now 1 :register 1} program))

(defn signal-strength [cycles cycle]
  (* cycle (get cycles (dec cycle))))

(defn visible? [pixel sprite]
  (get #{sprite (inc sprite) (dec sprite)} pixel))

(defn paint-line [line]
  (loop [[[i sprite] & xs] line
         out (vec (repeat 40 "."))]
    (if (some? i)
      (let [pixel (mod (dec i) 40)]
        (recur xs
          (if (visible? pixel (dec sprite))
            (assoc out pixel "#")
            out)))
      (apply str out))))

(defn part1 [cycles]
  (apply + (map #(signal-strength cycles %) output-idices)))

(defn part2 [cycles]
  (let [sorted (sort-by key (-> cycles (dissoc :now) (dissoc :register)))
        lines (partition 40 sorted)]
    (doseq [line (map paint-line lines)]
      (println line))))

(let [cycles (program->cycles (parse-input input))]
  (assert (= (part1 cycles) 13060))
  ;FJUBULRZ
  (part2 cycles))
