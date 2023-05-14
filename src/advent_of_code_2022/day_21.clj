(ns advent-of-code-2022.day_21
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def input (slurp "resources/day_21_input.txt"))

(defn extract-monkey-map [line]
  (let [[m1 m2 m3] (u/all-chars line)
        [_ op] (u/all-non-alphanumeric line)
        [num] (u/all-numbers line)]
    {m1 (if (nil? op)
          {:number num}
          {:left m2 :right m3 :op op})}))

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (map extract-monkey-map)
    (into {})))

(defn build-reference-map [monkey-map]
  (into {}
    (->> monkey-map
      (keep (fn [[k {:keys [number right left]}]]
              (when (nil? number)
                [[left k] [right k]])))
      (apply concat))))

(defn substitute [monkey-map]
  (let [reference-map (build-reference-map monkey-map)]
    (reduce-kv (fn [m k {:keys [number]}]
                 (if (some? number)
                   (update m (get reference-map k)
                     #(assoc % (if (= (:left %) k) :left :right) number))
                   m))
      monkey-map monkey-map)))

(defn simplify [monkey-map]
  (reduce-kv (fn [mm k {:keys [op right left]}]
               (if (and (number? left) (number? right))
                 (assoc mm k {:number ((resolve (symbol op)) left right)})
                 mm))
    monkey-map monkey-map))

(defn resolve-map [monkey-map]
  (let [iter-fn (comp simplify substitute)]
    (loop [current monkey-map]
      (let [resolved (iter-fn current)]
        (if (= current resolved)
          resolved
          (recur resolved))))))

(defn inverse-op [op]
  (case op
    "-" "+"
    "+" "-"
    "/" "*"
    "*" "/"))

(defn op->symbol [op side]
  (resolve
    (symbol
      (case side
        :left (inverse-op op)
        :right (if (some #{op} ["/" "-"]) op (inverse-op op))))))


(defn resolve-next [monkey-map k k-next sum]
  (let [{:keys [left right op]} (get monkey-map k)
        unknown (if (= left k-next) :left :right)
        resolved-op (op->symbol op unknown)]
    (case unknown
      :left (resolved-op sum right)
      :right (apply resolved-op
               (if (= op "-") [left sum] [sum left])))))


(defn get-path [monkey-map a b]
  (loop [path []
         current a]
    (let [[left right] ((juxt :left :right) (get monkey-map current))
          extended (conj path current)
          next (if (number? right) left right)]
      (condp = next
        b extended
        (recur extended next)))))

(defn path->sum [monkey-map path init]
  (loop [[curr next & rest] path
         sum init]
    (if (nil? next)
      (resolve-next monkey-map curr "humn" sum)
      (recur (conj rest next) (resolve-next monkey-map curr next sum)))))

(defn solve-side [monkey-map root]
  (if (some? (get-in monkey-map [(:left root) :number]))
    :left
    :right))

(defn other-side [side]
  (case side
    :left :right
    :right :left))

(defn part2 [monkey-map]
  (let [root (get monkey-map "root")
        partially-solved (resolve-map (dissoc monkey-map "root" "humn"))
        solved-side (solve-side partially-solved root)
        path (get-path partially-solved ((other-side solved-side) root) "humn")
        init-sum (get-in partially-solved [(solved-side root) :number])
        path-sum (path->sum partially-solved path init-sum)]
    path-sum))

(defn part1 [monkey-map]
  (get-in (resolve-map monkey-map) ["root" :number]))

(assert (= (part1 (parse-input input)) 62386792426088))
(assert (= (part2 (parse-input input)) 3876027196185))

(comment
  (def monkey-map (parse-input input))
  )
