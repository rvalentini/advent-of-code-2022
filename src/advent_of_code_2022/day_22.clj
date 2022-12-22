(ns advent-of-code-2022.day_22
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def input (slurp "resources/day_22_input.txt"))

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (partition-by #(= "" %))
    (take-nth 2)))

(def x-folds 3)
(def y-folds 4)
(def lines (parse-input input))
(def width (apply max (map count (first lines))))
(def height (count (first lines)))

(defn lines->grid [lines]
  (let [grid (u/build-matrix {:x-dim width :y-dim height :init-fn (constantly "o")})]
    (reduce (fn [grid [y line]]
              (reduce (fn [grid [x pos]]
                        (if (not= pos \space)
                          (u/mark-at-pos grid (str pos) [x y])
                          grid))
                grid
                (map-indexed vector line)))
      grid
      (map-indexed vector lines))))

(defn line->cmds [line]
  (->> line
    (partition-by #(Character/isDigit %))
    (map #(apply str %))
    (map u/str->number)))

(defn turn [mode direction]
  (case [mode direction]
    [:up "R"] :right
    [:up "L"] :left
    [:right "R"] :down
    [:right "L"] :up
    [:left "R"] :up
    [:left "L"] :down
    [:down "R"] :left
    [:down "L"] :right))

(defn line->path [line]
  (let [cmds (line->cmds line)]
    (loop [[x & xs] cmds
           mode :right
           path []]
      (cond
        (nil? x) path
        (number? x) (recur xs mode (concat path (take x (repeat mode))))
        :else (recur xs (turn mode x) path)))))

(defn start-pos [grid]
  [(u/first-index-of #(= % ".") (first grid)) 0])

(defn wrap [grid pos direction]
  (let [wrap-move #(-> %
                     (u/move direction)
                     ((fn [[x y]] [(mod x width) (mod y height)])))]
    (loop [pos pos]
      (let [next (wrap-move pos)]
        (case (u/get-pos grid next)
          "o" (recur next)
          "#" nil
          next)))))

(defn move [pos direction grid]
  (let [next (u/move pos direction)
        next-val (u/get-pos grid next)]
    (case next-val
      "." next
      "#" pos
      (or (wrap grid next direction) pos))))

(defn password [{:keys [pos facing]}]
  (let [[x y] pos
        dir->n {:right 0 :down 1 :left 2 :up 3}]
    (+ (* 1000 (inc y)) (* 4 (inc x)) (dir->n facing))))

(defn walk [grid path]
  (loop [grid grid
         [x & xs] path
         pos (start-pos grid)
         facing nil]
    (if (some? x)
      (recur grid xs (move pos x grid) x)
      {:pos pos :facing facing})))

(def edge->edge
  {[1 :left] [6 :left]
   [1 :up] [9 :left]
   [1 :right] [2 :left]
   [1 :down] [4 :up]
   [2 :left] [1 :right]
   [2 :up] [9 :down]
   [2 :right] [7 :right]
   [2 :down] [4 :right]
   [4 :left] [6 :up]
   [4 :up] [1 :down]
   [4 :right] [2 :down]
   [4 :down] [7 :up]
   [6 :left] [1 :left]
   [6 :up] [4 :left]
   [6 :right] [7 :left]
   [6 :down] [9 :up]
   [7 :left] [6 :right]
   [7 :up] [4 :down]
   [7 :right] [2 :right]
   [7 :down] [9 :right]
   [9 :left] [1 :up]
   [9 :up] [6 :down]
   [9 :right] [7 :down]
   [9 :down] [2 :up]})

(defn upper-left-pos [side]
  (let [x-offset (/ width x-folds)
        y-offset (/ height y-folds)]
    (nth (for [y (range 0 y-folds)
               x (range 0 x-folds)]
           [(* x x-offset) (* y y-offset)])
      side)))

(defn grid->grids [grid]
  (for [i (range 0 (* x-folds y-folds))]
    (let [upper-left (upper-left-pos i)]
      (u/sub-matrix {:upper-left-pos upper-left :x-dim (/ width x-folds) :y-dim (/ height y-folds) :matrix grid}))))

(defn pos->edge [[x y] width]
  (cond
    (neg? x) :left
    (neg? y) :up
    (>= x width) :right
    :else :down))

(defn update-directions [old-dir new-dir]
  (into {} (map (fn [[k v]] [k (new-dir v)]) old-dir)))

(defn rotate [old-edge new-edge old-dir]
  (let [invert {:up :down :left :right :right :left :down :up}
        counter-clock {:up :right :left :up :right :down :down :left}
        clock {:up :left :left :down :right :up :down :right}
        same {:up :up :left :left :right :right :down :down}]
    (update-directions
      old-dir
      (case [old-edge new-edge]
        [:up :up] invert
        [:up :right] clock
        [:up :down] same
        [:up :left] counter-clock
        [:right :up] counter-clock
        [:right :right] invert
        [:right :down] clock
        [:right :left] same
        [:down :up] same
        [:down :right] counter-clock
        [:down :down] invert
        [:down :left] clock
        [:left :up] clock
        [:left :right] same
        [:left :down] counter-clock
        [:left :left] invert))))

(defn pos-on-next-side [[x y] old-edge new-edge width directions]
  (let [new-directions (rotate old-edge new-edge directions)
        max (dec width)
        new-pos (case [old-edge new-edge]
                  [:up :up] [(- max x) 0]
                  [:up :right] [max (- max x)]
                  [:up :down] [x max]
                  [:up :left] [0 x]
                  [:right :up] [(- max y) 0]
                  [:right :right] [max (- max y)]
                  [:right :down] [y max]
                  [:right :left] [0 y]
                  [:down :up] [x 0]
                  [:down :right] [max x]
                  [:down :down] [(- max x) max]
                  [:down :left] [0 (- max x)]
                  [:left :up] [y 0]
                  [:left :right] [max y]
                  [:left :down] [(- max y) max]
                  [:left :left] [0 (- max y)])]
    {:new-pos new-pos :directions new-directions}))

(defn wrap-cube [grid cube pos side old-directions]
  (let [width (count (first grid))
        edge (pos->edge pos width)
        [side new-edge] (edge->edge [side edge])
        {:keys [new-pos directions]} (pos-on-next-side pos edge new-edge width old-directions)
        next-field (u/get-pos (cube side) new-pos)]
    (if (= next-field "#")
      nil
      {:new-side side :new-pos new-pos :new-directions directions})))

(defn original-pos [side [x y]]
  (let [[upper-left-x upper-left-y] (upper-left-pos side)]
    [(+ upper-left-x x)
     (+ upper-left-y y)]))

(defn move-cube [pos direction grid cube side old-directions]
  (let [current {:new-side side :new-pos pos :new-directions nil}
        next (u/move pos direction)
        next-val (u/get-pos grid next)]
    (case next-val
      nil (or (wrap-cube grid cube next side old-directions)
            current)
      "." (assoc current :new-pos next)
      "#" current)))

(defn walk-cube [cube path]
  (loop [grid (cube (apply min (map key cube)))
         side (apply min (map key cube))
         [p & ps] path
         dir-map {:up :up :left :left :right :right :down :down}
         pos (start-pos grid)
         facing nil]
    (if (some? p)
      (let [direction (dir-map p)
            {:keys [new-side new-pos new-directions]}
            (move-cube pos direction grid cube side dir-map)]
        (recur
          (cube new-side)
          new-side
          ps
          (or new-directions dir-map)
          new-pos
          direction))
      {:pos pos :facing facing :side side})))

(def sides #{1 2 4 6 7 9})
(def grid (lines->grid (first lines)))
(def path (line->path (first (second lines))))

(defn part1 [grid path]
  (password (walk grid path)))

(defn part2 [grid path]
  (let [grids (grid->grids grid)
        cube (into {} (keep-indexed (fn [i g] (when (sides i) [i g])) grids))
        {:keys [side facing pos]} (walk-cube cube path)
        original-pos (original-pos side pos)]
    (password {:pos original-pos :facing facing})))

(assert (= (part1 grid path) 103224))
(assert (= (part2 grid path) 189097))
