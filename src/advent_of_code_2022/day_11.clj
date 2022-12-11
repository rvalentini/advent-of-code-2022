(ns advent-of-code-2022.day_11)

(def monkeys
  {0 {:id 0
      :items [76 88 96 97 58 61 67]
      :op #(* % 19)
      :divisor 3
      :test #(if (zero? (mod % 3)) 2 3)
      :inspections 0}
   1 {:id 1
      :items [93, 71, 79, 83, 69, 70, 94, 98]
      :op #(+ % 8)
      :divisor 11
      :test #(if (zero? (mod % 11)) 5 6)
      :inspections 0}
   2 {:id 2
      :items [50, 74, 67, 92, 61, 76]
      :op #(* % 13)
      :divisor 19
      :test #(if (zero? (mod % 19)) 3 1)
      :inspections 0}
   3 {:id 3
      :items [76, 92]
      :op #(+ % 6)
      :divisor 5
      :test #(if (zero? (mod % 5)) 1 6)
      :inspections 0}
   4 {:id 4
      :items [74, 94, 55, 87, 62]
      :op #(+ % 5)
      :divisor 2
      :test #(if (zero? (mod % 2)) 2 0)
      :inspections 0}
   5 {:id 5
      :items [59, 62, 53, 62]
      :op #(* % %)
      :divisor 7
      :test #(if (zero? (mod % 7)) 4 7)
      :inspections 0}
   6 {:id 6
      :items [62]
      :op #(+ % 2)
      :divisor 17
      :test #(if (zero? (mod % 17)) 5 7)
      :inspections 0}
   7 {:id 7
      :items [85, 54, 53]
      :op #(+ % 3)
      :divisor 13
      :test #(if (zero? (mod % 13)) 4 0)
      :inspections 0}})

(def divisors [2 3 5 7 11 13 17 19 23])

(defn init-levels [n]
  (into {} (map (fn [d] [d (mod n d)]) divisors)))

(defn update-levels [mods op]
  (reduce (fn [config d] (update config d #(mod (op %) d)))
    mods divisors))

(defn inspect-with-relieve [{:keys [op test]} item]
  (let [worry-level (int (Math/floor (/ (op item) 3)))]
    {:levels worry-level
     :next-monkey (test worry-level)}))

(defn inspect [{:keys [op test divisor]} item]
  (let [levels (update-levels item op)]
    {:levels levels
     :next-monkey (test (get levels divisor))}))

(defn play [with-relieve? monkeys index]
  (reduce (fn [monkeys item]
            (let [monkey (get monkeys index)
                  {:keys [levels next-monkey]}
                  (if with-relieve?
                    (inspect-with-relieve monkey item)
                    (inspect monkey item))]
              (-> monkeys
                (update-in [next-monkey :items] conj levels)
                (update-in [index :items] #(subvec % 1))
                (update-in [index :inspections] inc))))
    monkeys (:items (get monkeys index))))

(defn round [with-relive? monkeys]
  (reduce (partial play with-relive?) monkeys (range 0 (count monkeys))))

(defn calc-monkey-business [monkeys]
  (->> (sort-by #(:inspections (second %)) (seq monkeys))
    reverse
    vals
    (map :inspections)
    (take 2)
    (apply *)))

(defn part1 [monkeys]
  (->> monkeys
    (iterate (partial round true))
    (take 21)
    last
    calc-monkey-business))

(defn part2 [monkeys]
  (->> monkeys
    (mapv (fn [[k v]] [k (update v :items #(mapv init-levels %))]))
    (into {})
    (iterate (partial round false))
    (take 10001)
    last
    calc-monkey-business))

(assert (= (part1 monkeys) 182293))
(assert (= (part2 monkeys) 54832778815))
