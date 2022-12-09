(ns advent-of-code-2022.day_09
  (:require [clojure.string :as s]))

(def input (slurp "resources/day_09_input.txt"))

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (mapv #(s/split % #" "))
    (mapv (fn [[d t]] [d (parse-long (str t))]))))

(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])
(defn up [[x y]] [x (dec y)])
(defn down [[x y]] [x (inc y)])
(defn upper-left [[x y]] [(dec x) (dec y)])
(defn upper-right [[x y]] [(inc x) (dec y)])
(defn lower-left [[x y]] [(dec x) (inc y)])
(defn lower-right [[x y]] [(inc x) (inc y)])


(defn move-up [[[hx hy] [tx ty]]]
  [[hx (dec hy)]
   (condp = [tx ty]
     (lower-left [hx hy])  (upper-right [tx ty])
     (down [hx hy])        (up [tx ty])
     (lower-right [hx hy]) (upper-left [tx ty])
     [tx ty])])

(assert (= (move-up [[4 3] [3 4]]) [[4 2] [4 3]]))
(assert (= (move-up [[4 1] [4 2]]) [[4 0] [4 1]]))

(defn move-down [[[hx hy] [tx ty]]]
  [[hx (inc hy)]
   (condp = [tx ty]
     (upper-left [hx hy])  (lower-right [tx ty])
     (up [hx hy])          (down [tx ty])
     (upper-right [hx hy]) (lower-left [tx ty])
     [tx ty])])

(assert (= (move-down [[5 1] [4 1]]) [[5 2] [4 1]]))
(assert (= (move-down [[3 3] [3 2]]) [[3 4] [3 3]]))

(defn move-left [[[hx hy] [tx ty]]]
  [[(dec hx) hy]
   (condp = [tx ty]
     [(inc hx) (dec hy)] (lower-left [tx ty])
     [(inc hx) hy]       (left [tx ty])
     [(inc hx) (inc hy)] (upper-left [tx ty])
     [tx ty])])

(assert (= (move-left [[2 2] [3 3]]) [[1 2] [2 2]]))

(defn move-right [[[hx hy] [tx ty]]]
  [[(inc hx) hy]
   (condp = [tx ty]
     [(dec hx) (dec hy)] (lower-right [tx ty])
     [(dec hx) hy]       (right [tx ty])
     [(dec hx) (inc hy)] (upper-right [tx ty])
     [tx ty])])

(defn move-upper-right [[[hx hy] [tx ty]]]
  [(upper-right [hx hy])
   (condp = [tx ty]
     [(dec hx) (dec hy)] (right [tx ty])
     [(dec hx) hy]       (upper-right [tx ty])
     [(dec hx) (inc hy)] (upper-right [tx ty])
     [hx (inc hy)]       (upper-right [tx ty])
     [(inc hx) (inc hy)] (up [tx ty])
     [tx ty])])

(defn move-upper-left [[[hx hy] [tx ty]]]
  [[(dec hx) (dec hy)]
   (condp = [tx ty]
     [(inc hx) (dec hy)] (left [tx ty])
     [(inc hx) hy]       (upper-left [tx ty])
     [(dec hx) (inc hy)] (up [tx ty])
     [hx (inc hy)]       (upper-left [tx ty])
     [(inc hx) (inc hy)] (upper-left [tx ty])
     [tx ty])])


(defn move-lower-left [[[hx hy] [tx ty]]]
  [(lower-left [hx hy])
   (condp = [tx ty]
     [(dec hx) (dec hy)]  (down [tx ty])
     [hx (dec hy)]        (lower-left [tx ty])
     [(inc hx) (dec hy)]  (lower-left [tx ty])
     [(inc hx) hy]        (lower-left [tx ty])
     [(inc hx) (inc hy)]  (left [tx ty])
     [tx ty])])

(defn move-lower-right [[[hx hy] [tx ty]]]
  [(lower-right [hx hy])
   (condp = [tx ty]
     (upper-left [hx hy])  (lower-right [tx ty])
     (up [hx hy])          (lower-right [tx ty])
     (upper-right [hx hy]) (down [tx ty])
     (left [hx hy])        (lower-right [tx ty])
     (lower-left [hx hy])  (right [tx ty])
     [tx ty])])

(assert (= (move-right [[2 2] [1 3]]) [[3 2] [2 2]]))

(defn move [d ht]
  (case d
    "U" (move-up ht)
    "D" (move-down ht)
    "L" (move-left ht)
    "R" (move-right ht)))


(defn get-dir-fn [old new]
  (condp = new
    (up old) move-up
    (down old) move-down
    (left old) move-left
    (right old) move-right
    (upper-left old) move-upper-left
    (upper-right old) move-upper-right
    (lower-left old) move-lower-left
    (lower-right old) move-lower-right))

(defn move-all [d [h t & rest :as old-rope]]
  (let [old-rope (vec old-rope)
        [new-h new-t] (move d [h t])
        rest (vec (concat [new-t] rest))]
    (loop [[x y & xs :as remaining] rest
           i 1
           new-rope [new-h]]
      (cond
        (and (some? x) (nil? y)) (conj new-rope x)

        (not= x (get old-rope i))
        (let [dir-fn (get-dir-fn (get old-rope i) x)
              next (second (dir-fn [(get old-rope i) y]))]
          (recur (vec (concat [next] xs)) (inc i) (conj new-rope x)))

        :else (vec (concat new-rope remaining))))))

(defn collect-seen [h t moves]
  (loop [[x & xs] moves
         ht [h t]
         seen []]
    (if (some? x)
      (let [result (move x ht)]
        (recur xs result (conj seen result)))
      (apply hash-set (map second seen)))))

(defn part1 [start moves]
  (->> moves
    (collect-seen start start)
    count))

(defn part2 [start moves]
  (let [rope (repeat 10 start)]
    (loop [[x & xs] moves
           seen []
           rope rope]
      (if (some? x)
        (let [new-rope (move-all x rope)]
          (recur xs (conj seen (peek new-rope)) new-rope))
        (count (apply hash-set seen))))))


(comment
  (def moves (->> (parse-input input)
               (map (fn [[d t]] (repeat t d)))
               (apply concat)))

  ; 6503
  (part1 [0 4] moves)
  ; 2724
  (part2 [11 15] moves)


  )







