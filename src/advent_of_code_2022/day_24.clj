(ns advent-of-code-2022.day_24
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def input (slurp "resources/day_24_input.txt"))

(defn wall? [x-dim y-dim [x y]]
  (or (zero? y) (zero? x)
    (= x (dec x-dim)) (= y (dec y-dim))))

(defn arrow->dir [arrow]
  (case arrow
    "^" :up
    "<" :left
    ">" :right
    "v" :down
    nil))

(defn wrap-around [x-dim y-dim [x y]]
  (cond
    (zero? x) [(- x-dim 2) y]
    (zero? y) [x (- y-dim 2)]
    (= x (dec x-dim)) [1 y]
    (= y (dec y-dim)) [x 1]))

(defn next-pos [x-dim y-dim pos arrow]
  (when-let [direction (arrow->dir arrow)]
    (let [next (u/move pos direction)]
      (if (and (not= pos next) (wall? x-dim y-dim next))
        (wrap-around x-dim y-dim next)
        next))))

(defn apply-updates [updates org-pos m]
  (reduce
    (fn [m [pos sym]]
      (-> m
        (update org-pos (fn [xs] (u/drop-first #(= % sym) xs)))
        (update pos conj sym)))
    m
    updates))

(defn tick [x-dim y-dim m]
  (reduce
    (fn [m [org-pos syms]]
      (let [updates (for [s syms
                          :let [next-pos (next-pos x-dim y-dim org-pos s)]
                          :when (some? next-pos)]
                      [next-pos s])]
        (apply-updates updates org-pos m)))
    m m))

(defn print-basin [xs]
  (let [max-x (apply max (map first (keys xs)))
        max-y (apply max (map second (keys xs)))
        matrix (u/build-matrix {:x-dim (inc max-x)
                                :y-dim (inc max-y)
                                :init-fn (constantly ".")})]
    (reduce
      (fn [matrix [pos syms]]
        (if (> (count syms) 1)
          (u/mark-at-pos matrix (str (count syms)) pos)
          (u/mark-at-pos matrix (or (first syms) ".") pos)))
      matrix
      xs)))

(defn compact [xs]
  (reduce
    (fn [s [pos sym]]
      (if (s pos)
        (update s pos conj sym)
        (assoc s pos [sym])))
    {}
    xs))

(defn pos->syms [matrix]
  (let [dim-x (count (first matrix))
        dim-y (count matrix)]
    (compact (for [x (range 0 dim-x)
                   y (range 0 dim-y)
                   :when (not= "." (u/get-pos matrix [x y]))]
               [[x y]
                (u/get-pos matrix [x y])]))))

(defn parse-input [input]
  (-> input
    s/split-lines
    u/lines->matrix
    pos->syms))

(comment
  (def m (parse-input input))
  m
  (def dim-x 8)
  (def dim-y 6)
  (tick dim-x dim-y m)

  (print-basin m)
  (->> m
    (iterate (partial tick dim-x dim-y))
    (take 19)
    last
    print-basin)

  )
