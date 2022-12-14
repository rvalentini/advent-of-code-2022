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

(def ^:private inf Double/POSITIVE_INFINITY)

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisited
  neighbors by using curr's shortest path"
  [g costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
      (fn [c nbr nbr-cost]
        (if (unvisited nbr)
          (update-in c [nbr] min (+ curr-cost nbr-cost))
          c))
      costs
      (get g curr))))

(defn dijkstra
  "Returns a map of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a map of nodes to map of neighboring nodes and associated cost.
  Optionally, specify destination node to return once cost is known"
  ([g src]
   (dijkstra g src nil))
  ([g src dst]
   (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
          curr src
          unvisited (disj (apply hash-set (keys g)) src)]
     (cond
       (= curr dst)
       (select-keys costs [dst])

       (or (empty? unvisited) (= inf (get costs curr)))
       costs

       :else
       (let [next-costs (update-costs g costs unvisited curr)
             next-node (apply min-key next-costs unvisited)]
         (recur next-costs next-node (disj unvisited next-node)))))))



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
