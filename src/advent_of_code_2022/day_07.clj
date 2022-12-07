(ns advent-of-code-2022.day_07
  (:require [clojure.string :as s]
            [advent-of-code-2022.util :as u]))

(def input (slurp "resources/day_07_input.txt"))
(def disk-size 70000000)

(defn pop-dir [path]
  (let [remaining (reverse (rest (drop-while #(not= \/ %) (reverse path))))]
    (if (not-empty remaining)
      (apply str remaining)
      "/")))

(defn push-dir [path dir]
  (if (= path "/")
    (str path dir)
    (str path "/" dir)))

(defn path->keys [path]
  (let [dirs (vec (map #(str "/" %) (rest (s/split path #"/"))))
        keys (loop [keys []
                    i 0]
               (if (< i (count dirs))
                 (recur (conj keys (apply str (subvec dirs 0 (inc i)))) (inc i))
                 keys))]
    (vec (conj keys "/"))))

(defn parse-file-size [f]
  (let [[size _] (s/split f #" ")]
    (parse-long size)))

(defn sum-dirs [fs]
  (reduce
    (fn [m [path size]]
      (reduce
        (fn [m k]
          (update m k #(+ (or % 0) size))) m (path->keys path))) {} fs))

(defn file? [name]
  (Character/isDigit (.charAt name 0)))

(defn parse-input [input]
  (loop [[x & xs] (s/split-lines input)
         fs []
         path "/"]
    (if (some? x)
      (cond
        (s/starts-with? x "$ cd ..") (recur xs fs  (pop-dir path))
        (s/starts-with? x "$ cd /") (recur xs fs  "/")
        (s/starts-with? x "$ cd") (recur xs fs  (push-dir path (last (u/all-chars x))))
        (file? x) (recur xs (conj fs [path (parse-file-size x)]) path)
        :else (recur xs fs path))
      fs)))

(defn find-smallest [dirs required-size]
  (->> dirs
    (sort-by val)
    (drop-while (fn [[_ s]] (<= s required-size)))
    first))

(defn part1 [dirs]
  (reduce-kv (fn [acc _ s] (+ acc s)) 0
    (filter (fn [[_ size]] (<= size 100000))
      dirs)))

(defn part2 [dirs]
  (let [free (- disk-size (get dirs "/"))
        still-required (- 30000000 free)]
    (second (find-smallest dirs still-required))))

(let [dirs (sum-dirs
             (parse-input input))]
  (println "Sum of all dirs below threshold:" (part1 dirs))
  (println "Smallest required dir:" (part2 dirs)))
