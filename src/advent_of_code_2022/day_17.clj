(ns advent-of-code-2022.day_17
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def input (slurp "resources/day_17_input.txt"))

(defn arrow->dir [a]
  (case a
    "<" :left
    ">" :right))

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (map #(s/split % #""))
    (map #(map arrow->dir %))
    first))

(defn shape1 [[x y :as left-bottom]]
  [left-bottom [(inc x) y] [(+ 2 x) y] [(+ 3 x) y]])

(defn shape2 [[x y :as left-bottom]]
  [[x (dec y)] [(inc x) (dec y)] [(inc x) y]  [(inc x) (- y 2)] [(+ x 2) (dec y)]])

(defn shape3 [[x y :as left-bottom]]
  [left-bottom [(inc x) y] [(+ x 2) y] [(+ x 2) (dec y)] [(+ x 2) (- y 2)]])

(defn shape4 [[x y :as left-bottom]]
  [left-bottom [x (dec y)] [x (- y 2)] [x (- y 3)]])

(defn shape5 [[x y :as left-bottom]]
  [left-bottom [x (dec y)] [(inc x) (dec y)] [(inc x) y] ])


(defn collides? [fixed bottom width rock]
  (some (some-fn
          (fn [pos] (get fixed pos))
          (fn [[_ y]] (>= y bottom))
          (fn [[x _]] (or (< x 0) (>= x width))))
    rock))

;(collides? #{[3 3] [4 3]} 20 7 (shape1 [3 4]))


(defn move [fixed bottom width rock direction]
  ;(println "---------------" )
  ;(println "bottom" rock )
  ;(println "width" rock )
  ;(println "rock" rock )
  ;(println "fixed" fixed )
  (let [new-rock (case direction
                    :left (map (fn [[x y]] [(dec x) y]) rock)
                    :right (map (fn [[x y]] [(inc x) y]) rock)
                    :down (map (fn [[x y]] [x (inc y)]) rock))]
    (if (collides? fixed bottom width new-rock)
      nil
      new-rock)))



(defn calc-next-start [bottom fixed]
  [2 (- (reduce
          (fn [min [_ y]] (if (< y min) y min))
          bottom
          fixed) 4)])

(defn calc-tower-height [bottom fixed]
  (- bottom (reduce
              (fn [min [_ y]] (if (< y min) y min))
              bottom
              fixed)))


(defn print-state [bottom width active-rock fixed]
  (let [chamber (u/build-matrix {:x-dim width :y-dim bottom :init-fn (constantly ".")})
        with-active (reduce (fn [c pos] (u/mark-at-pos c "@" pos)) chamber active-rock)
        with-fixed (reduce (fn [c pos] (u/mark-at-pos c "#" pos)) with-active fixed)]
    (doseq [l with-fixed]
      (println l))))

(def directions (parse-input input))

(defn play-tetris [bottom width directions num-rocks]
  (let [start-pos [2 (- bottom 4)]]
    (loop [[next-shape & sxs :as shapes] (rest (cycle [shape1 shape2 shape3 shape4 shape5]))
           active-rock (shape1 start-pos)
           [next-dir & dxs] (cycle directions)
           fixed #{}
           rock-count 0
           iter-count 0
           ]
      (when (= next-shape shape1) (println "count when shape1 comes up" iter-count))
      ;(println "hight: " (calc-tower-height bottom fixed))
      #_(when (= (mod rock-count 50455) 0) (println "After " rock-count "rocks: " (calc-tower-height bottom fixed)))
      #_(print-state bottom width active-rock fixed)
      (if (= rock-count num-rocks)
        (calc-tower-height bottom fixed)
        (let [maybe-side-move (or (move fixed bottom width active-rock next-dir) active-rock)]
          (if-let [next (move fixed bottom width maybe-side-move :down)]
            (recur shapes next dxs fixed rock-count (inc iter-count))
            (let [new-fixed (apply conj fixed maybe-side-move)]
              ;(println "rock-count:" rock-count "iter-count: " iter-count)
              (recur
                sxs
                (next-shape (calc-next-start bottom new-fixed))
                dxs
                new-fixed
                (inc rock-count)
                0))))))))


(defn rock-count-to-height [drop-count]
  (drop drop-count (map-indexed (fn [idx r-count] [idx (play-tetris 20 7 directions r-count)]) (range 0 10000))))


(defn idx-to-height-increase [rock-count-to-height] (map
                              (fn [[n-1 n]] [(first n) (- (second n) (second n-1))])
                              (map
                                vector
                                rock-count-to-height
                                (rest rock-count-to-height))))


(doseq [drop-count (range 0 1000)]
  (let [rcth (rock-count-to-height drop-count)
        ithi (idx-to-height-increase rcth)]
    (println "drop count: " drop-count)
    (doseq [partition-size (range 1 500)]
      (let [[first second] (partition partition-size (map second ithi))]
        (when (= first second) (println "same partitions for size" partition-size))))
    )
  )

;(count directions)


#_(doseq [partition-size (range 1 80)]
  (let [[first second] (partition partition-size (map second idx-to-height-increase))]
    (when (= first second) (println "same partitions for size" partition-size))))

(play-tetris 20 7 directions 15) -> 25
(play-tetris 20 7 directions 35)
(play-tetris 20 7 directions 70)
(play-tetris 20 7 directions 105)

; 35 cylce lenght
; 53 increase of height per cycle
; 25 height of first drop
(+ 25 (* (/ (- 1000000000000 15) 35) 53))

;(* 5 (count directions))
;
;(- 77 54)
;(- 87 62)
;(- 81 58)
;(- 33 14)
;
;0

; full kgv loops 19819641

;1580398353700
;(* 2 15795)
;15795
;31597

(comment
  (def lines (parse-input input))
  lines

  )
