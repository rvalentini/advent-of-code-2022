(ns advent-of-code-2022.day_19
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "resources/day_19_input.txt"))

(def test-blueprint {1 {:ore {:ore 4}
                        :clay {:ore 2}
                        :obsidian {:ore 3 :clay 14}
                        :geode {:ore 2 :obsidian 7}}
                     2 {:ore {:ore 2}
                        :clay {:ore 3}
                        :obsidian {:ore 3 :clay 8}
                        :geode {:ore 3 :obsidian 12}}})

(defn nums->blueprint [nums]
  {(nth nums 0) {:ore {:ore (nth nums 1)}
                 :clay {:ore (nth nums 2)}
                 :obsidian {:ore (nth nums 3) :clay (nth nums 4)}
                 :geode {:ore (nth nums 5) :obsidian (nth nums 6)}}})

(defn build-possible? [resources blueprint robot]
  (case robot
    :ore (>= (:ore resources) (get-in blueprint [:ore :ore]))
    :clay (>= (:ore resources) (get-in blueprint [:clay :ore]))
    :obsidian (and
                (>= (:ore resources) (get-in blueprint [:obsidian :ore]))
                (>= (:clay resources) (get-in blueprint [:obsidian :clay])))
    :geode (and
             (>= (:ore resources) (get-in blueprint [:geode :ore]))
             (>= (:obsidian resources) (get-in blueprint [:geode :obsidian])))
    :none true
    :else :error))

(defn build-robot [resources blueprint robots robot]
  ;(println "build robot called with")
  ;(println "resources" resources)
  ;(println "blueprint" blueprint)
  ;(println "robots" robots)
  ;(println "robot" robot)
  [(case robot
     :ore (update resources :ore #(- % (get-in blueprint [:ore :ore])))
     :clay (update resources :ore #(- % (get-in blueprint [:clay :ore])))
     :obsidian (-> resources
                 (update :ore #(- % (get-in blueprint [:obsidian :ore])))
                 (update :clay #(- % (get-in blueprint [:obsidian :clay]))))
     :geode (-> resources
              (update :ore #(- % (get-in blueprint [:geode :ore])))
              (update :obsidian #(- % (get-in blueprint [:geode :obsidian])))))
   (update robots robot inc)])

(defn collect-resources [resources robots]
  (reduce-kv (fn [resources robot count]
               (update resources robot #(+ % count)))
    resources robots))



;(defn simple-order-plan [resources blueprint robots]
;  (let [order [:geode :obsidian :clay :ore]]
;    (loop [resources resources
;           robots robots]
;      (if (build-possible? resources blueprint (nth order 0))
;        (let [[resources robots] (build-robot resources blueprint robots (nth order 0))]
;          (recur resources robots))
;        (if (build-possible? resources blueprint (nth order 1))
;          (let [[resources robots] (build-robot resources blueprint robots (nth order 1))]
;            (recur resources robots))
;          (if (build-possible? resources blueprint (nth order 2))
;            (let [[resources robots] (build-robot resources blueprint robots (nth order 2))]
;              (recur resources robots))
;            (if (build-possible? resources blueprint (nth order 3))
;              (let [[resources robots] (build-robot resources blueprint robots (nth order 3))]
;                (recur resources robots))
;              [resources robots])))))))                     ;TODO use some-fn
;
;(defn simple-order-single-plan [resources blueprint robots]
;  (let [order [:geode :obsidian :clay :ore]]
;    (if (build-possible? resources blueprint (nth order 0))
;      (build-robot resources blueprint robots (nth order 0))
;      (if (build-possible? resources blueprint (nth order 1))
;        (build-robot resources blueprint robots (nth order 1))
;        (if (build-possible? resources blueprint (nth order 2))
;          (build-robot resources blueprint robots (nth order 2))
;          (if (build-possible? resources blueprint (nth order 3))
;            (build-robot resources blueprint robots (nth order 3))
;            [resources robots]))))))
;
;(defn patient-plan [resources blueprint robots]
;  (if (and (< 0 (:ore robots)) (< 0 (:clay robots)))
;    (if (build-possible? resources blueprint :obsidian)
;      (build-robot resources blueprint robots :obsidian)
;      (if (build-possible? resources blueprint :geode)
;        (build-robot resources blueprint robots :geode)
;        [resources robots]))
;    (if (build-possible? resources blueprint :clay)
;      (build-robot resources blueprint robots :clay)
;      [resources robots])))

{:path []
 :resources {}
 :robots {}}

(defn step [resources blueprint robots robot]
  (let [[res-after-build new-robots] (if (= :none robot)
                                       [resources robots]   ;TODO make nicer
                                       (build-robot resources blueprint robots robot))
        res-after-collect (collect-resources res-after-build robots)]
    [res-after-collect new-robots]))


(concat '(1 2 3) '(4 5 6))

(into [{:a 1} {:b 1} {:c 1}] '({:f 55} {:k 22}))
(into '({:a 1} {:b 1} {:c 1}) '({:f 55} {:k 22}))

(defn reduce-candidates [candidates branch blueprint]
  (cond
    (= (count (:path branch)) 20)                           ;second to last iteration -> only makes sense to build geode
    (if (build-possible? (:resources branch) blueprint :geode)
      [:geode]
      [:none])

    (build-possible? (:resources branch) blueprint :geode)
    [:geode]

    :else
    (filter #(build-possible? (:resources branch) blueprint %) candidates)))

;TODO destructure branch
(defn find-optimal-path [blueprint resources robots]
  (let [init-branch {:path []
                     :resources resources
                     :robots robots}]
    (loop [[branch & bs] [init-branch]
           finished []
           geode-max 0
           iter 0]
      (when (= (mod iter 10000000) 0)
        (println "iteration" (/ iter 1000000) "mio")
        (println "current branch" branch)
        (println "current geode-max" geode-max))
      ;(println "branch" branch)
      ;(println "finished" finished)
      (if (nil? branch)
        finished
        (if (= (count (:path branch)) 21)
          (let [geode-count (get-in branch [:resources :geode])]
            (recur
              bs
              (conj finished {:path (:path branch)
                              :geodes geode-count})
              (max geode-max geode-count)
              (inc iter)))
          (let [candidates [:geode :obsidian :clay :ore :none]
                possible-candidates (reduce-candidates candidates branch blueprint)
                new-paths (reduce (fn [paths c]
                                    (let [[res robs] (step (:resources branch) blueprint (:robots branch) c)]
                                      (conj paths
                                        {:path (conj (:path branch) c)
                                         :resources res
                                         :robots robs})))
                            []
                            possible-candidates)]
            (recur (into new-paths bs) finished geode-max (inc iter))))))))




(def resources {:ore 2
                :clay 0
                :obsidian 0
                :geode 0})
(def robots {:ore 1
             :clay 0
             :obsidian 0
             :geode 0})

; optimization: first two are always :none - cannot build anything
; optimization: last one is always :none - doesn't make sense to build anything
; --> path lenght is 24 -2 -1 = 21
(find-optimal-path (get test-blueprint 1) resources robots)

;(defn run [blueprint build-plan]
;  (loop [resources {:ore 0
;                    :clay 0
;                    :obsidian 0
;                    :geode 0}
;         robots {:ore 1
;                 :clay 0
;                 :obsidian 0
;                 :geode 0}
;         minute 1]
;    (println "--- minute" minute "---")
;    (println "robots" robots)
;    (println "resources" robots)
;    (if (<= minute 24)
;      (let [[res-after-build new-robots] (build-plan resources blueprint robots)
;            res-after-collect (collect-resources res-after-build robots)]
;        (recur res-after-collect new-robots (inc minute)))
;      resources)))

(defn parse-input [input]
  (->> input
    (s/split-lines)
    (map u/all-numbers)
    (map nums->blueprint)))


(comment
  (def lines (parse-input input))
  lines

  test-blueprint
  ;(run (get test-blueprint 1) patient-plan)
  ;(run (get test-blueprint 2) simple-order-plan)

  (doseq [plan (combo/permutations [:clay :obsidian :geode :ore])]
    (println "Plan " plan " result " (run (get test-blueprint 1) plan)))

  )
