(ns advent-of-code-2022.util)

(defn all-chars [str]
  (vec (re-seq #"[A-Za-z]+" str)))

(defn all-numbers [str]
  (mapv parse-long (re-seq #"-?[0-9]+" str)))

(defn first-index-of [p xs]
  (->> xs
    (keep-indexed (fn [i x] (when (p x) i)))
    first))

(defn first-repeated [[x & xs]]
  (if (= x (first xs))
    x
    (recur xs)))

(defn neighbors [[x y]]
  (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]))

(defn neighbors-with-corners [[x y :as pos]]
  (filter #(not= % pos)
    (for [dx [0 -1 1]
          dy [0 -1 1]]
      [(+ x dx) (+ y dy)])))

(defn build-matrix [{:keys [x-dim y-dim init-fn]}]
  (vec (map vec (for [y (range 0 y-dim)]
                  (for [x (range 0 x-dim)]
                    (init-fn x y))))))

(defn mark-at-pos [matrix i [x y]]
  (assoc-in matrix [y x] i))

(defn mark-row [matrix i row]
  (map-indexed (fn [idx r] (if (= idx row) (vec (map (constantly i) r)) r)) matrix))

(defn mark-column [matrix i col]
  (map (fn [r] (map-indexed (fn [idx c] (if (= idx col) i c)) r)) matrix))


;testy test
(assert (= (all-numbers "blah -333") [-333]))
(assert (= (all-numbers "blah -8 5 0 something -22 else  111") [-8 5 0 -22 111]))
(assert (= (all-numbers "blah -10") [-10]))
(assert (= (all-numbers "-1u0m22-1") [-1 0 22 -1]))

(assert (= (count (flatten (build-matrix {:x-dim 20 :y-dim 5 :init-fn (constantly ".")}))) 100 ))
;sum of first 100 numbers
(assert (= (apply + (flatten (build-matrix {:x-dim 20 :y-dim 5 :init-fn (fn [x y] (+ (inc x) (* y 20)))})))
          5050))

(assert (= (-> (build-matrix {:x-dim 20 :y-dim 5 :init-fn (constantly ".")})
             (mark-row "x" 3)
             flatten
             ((fn [s] (filter #(= % "x") s)))
             count)
          20))

(assert (= (-> (build-matrix {:x-dim 20 :y-dim 5 :init-fn (constantly ".")})
             (mark-column "x" 7)
             flatten
             ((fn [s] (filter #(= % "x") s)))
             count)
          5))

(assert (= :d (first-repeated '(:a :b :c :d :d :e :f))))

(assert (= '([1 0] [1 2] [0 1] [0 0] [0 2] [2 1] [2 0] [2 2])
          (neighbors-with-corners [1 1])))
