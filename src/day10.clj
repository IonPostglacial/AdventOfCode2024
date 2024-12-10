(ns day10 (:require [clojure.string :as str]))

(def input (slurp "input/day10.txt"))

(def lines (str/split-lines input))
(def graph (mapv #(mapv parse-long (str/split % #"")) lines))

(defn find-trail-heads [topo-map]
  (into [] (for [y (range (count graph))
                 x (range (count (first graph)))
                 :let [pos [y x]]
                 :when (= 0 (get-in topo-map pos))]
             pos)))

(def trail-heads (find-trail-heads graph))

(def deltas [[0 -1] [0 1] [-1 0] [1 0]])

(defn adding-point [[px py]]
  (fn [[dx dy]] [(+ px dx) (+ py dy)]))

(defn trails [topo-map head]
  (loop [peaks []
         frontier [head]
         explored #{}]
    (if (empty? frontier)
      peaks
      (let [[pos & next-frontier] frontier
            value (get-in topo-map pos)
            valid-neighbor? #(and (= (inc value) (get-in topo-map %))
                                  (not (contains? explored %)))
            neighbors (into [] (comp (map (adding-point pos))
                                     (filter valid-neighbor?)) deltas)]
        (cond (= value 8) (recur (into peaks neighbors)
                                 next-frontier
                                 (into explored neighbors))
              :else (recur peaks
                           (into next-frontier neighbors)
                           (into explored neighbors)))))))

(println "Solution 1:"
         (reduce + (map #(count (trails graph %)) trail-heads)))