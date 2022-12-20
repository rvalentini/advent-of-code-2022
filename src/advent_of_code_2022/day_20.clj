(ns advent-of-code-2022.day_20
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def input (slurp "resources/day_20_input.txt"))

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (map parse-long)))


(#{1 2 3} 3)
(conj #{1 2 3} 5)
(random-uuid)

(defn substitute-duplicates [numbers]
  (loop [[n & ns] numbers
         result '()
         substitution-map {}
         seen #{}]
    (if (nil? n)
      [(reverse result) substitution-map]
      (if (seen n)
        (let [sub (random-uuid)]
          (recur ns (conj result sub) (assoc substitution-map sub n) seen))
        (recur ns (conj result n) substitution-map (conj seen n))))))


(defn numbers->map [numbers key]
  (case key
    :num (into {} (map-indexed (fn [i n] [n i]) numbers))
    :pos (into {} (map-indexed vector numbers))))

(defn shift-right [position pos->num num->pos]
  (let [num-at-pos (get pos->num position)
        next-pos (mod (inc position) (count pos->num))
        num-at-next-pos (get pos->num next-pos)]
    [(-> pos->num
       (assoc position num-at-next-pos)
       (assoc next-pos num-at-pos))
     (-> num->pos
       (assoc num-at-pos next-pos)
       (assoc num-at-next-pos position))]))

;TODO simplify
(defn shift-left [position pos->num num->pos]
  (let [num-at-pos (get pos->num position)
        prev-pos (mod (dec position) (count pos->num))
        num-at-prev-pos (get pos->num prev-pos)]
    [(-> pos->num
       (assoc position num-at-prev-pos)
       (assoc prev-pos num-at-pos))
     (-> num->pos
       (assoc num-at-pos prev-pos)
       (assoc num-at-prev-pos position))]))

(defn grove-coordinates [pos->num num->pos]
  (let [zero-pos (get num->pos 0)]
    ;(println "zero-pos:" zero-pos)
    [(get pos->num (mod (+ zero-pos 1000) (count pos->num)))
     (get pos->num (mod (+ zero-pos 2000) (count pos->num)))
     (get pos->num (mod (+ zero-pos 3000) (count pos->num)))]))

(let [[numbers subs-map] (substitute-duplicates (parse-input input))]
  (def numbers numbers)
  (def subs-map subs-map))

(def pos->num (numbers->map numbers :pos))
(def num->pos (numbers->map numbers :num))

(count pos->num)
(count num->pos)
num->pos
pos->num

(let [[pos->num num->pos] (reduce (fn [[pos->num num->pos] n]
                                    (if (zero? (if (number? n) n (subs-map n)))
                                      [pos->num num->pos]
                                      (reduce (fn [[pos->num num->pos] _] ;TODO use iterate
                                                (let [current-pos (get num->pos n)]
                                                  (if (neg? (if (number? n) n (subs-map n)))
                                                    (shift-left current-pos pos->num num->pos)
                                                    (shift-right current-pos pos->num num->pos))))
                                        [pos->num num->pos]
                                        (range 0 (Math/abs (if (number? n) n (subs-map n)))))))
                            [pos->num num->pos]
                            numbers)]
  (def out pos->num)
  (println (map second pos->num))
  (println (count (map second pos->num)))
  (println "grove coords: " (grove-coordinates pos->num num->pos))
  (println "sum grove: " (apply + (map #(if (number? %) % (subs-map %)) (grove-coordinates pos->num num->pos))))

  )

;(take 3 (iterate (partial (if (neg? 5) shift-left shift-right) 2) pos->num))






;(assert (=  [{0 1, 1 2, 2 -3, 3 -2, 4 3, 5 0, 6 4}
;             {1 0, 2 1, -3 2, 3 4, -2 3, 0 5, 4 6}] (shift-right 3 pos->num num->pos)))
;(assert (=  [{0 4, 1 2, 2 -3, 3 3, 4 -2, 5 0, 6 1}
;             {1 6, 2 1, -3 2, 3 3, -2 4, 0 5, 4 0}] (shift-right 6 pos->num num->pos)))
;
;(assert (=  [{0 1, 1 2, 2 -3, 3 -2, 4 3, 5 0, 6 4}
;             {1 0, 2 1, -3 2, 3 4, -2 3, 0 5, 4 6}] (shift-left 4 pos->num num->pos)))
;(assert (=  [{0 4, 1 2, 2 -3, 3 3, 4 -2, 5 0, 6 1}
;             {1 6, 2 1, -3 2, 3 3, -2 4, 0 5, 4 0}] (shift-left 0 pos->num num->pos)))

numbers
pos->num

(comment
  (def numbers (parse-input input))
  numbers
  (pos->num numbers)

  )
