(ns day6
  (:require [clojure.string :as str]))

(def grid (str/split-lines
           (slurp "input/day6.txt")))

(defn parse-level [grid]
  (let [height (count grid)
        width (count (first grid))]
    (loop [i 0 j 0 guard nil obstacles #{}]
      (cond (> i width) (recur 0 (inc j) guard obstacles)
            (> j height) {:guard guard :obstacles obstacles :width width :height height}
            (= \# (get-in grid [j i])) (recur (inc i) j guard (conj obstacles [i j]))
            (= \^ (get-in grid [j i])) (recur (inc i) j [i j] obstacles)
            :else (recur (inc i) j guard obstacles)))))

(defn next-direction [direction]
  (case direction
    [1 0] [0 1]
    [0 1] [-1 0]
    [-1 0] [0 -1]
    [0 -1] [1 0]))

(defn next-position [[x y] [dx dy]] [(+ x dx) (+ y dy)])

(defn out-of-bounds? [[x y] width height]
  (or (< x 0) (>= x (dec width))
       (< y 0) (>= y (dec height))))

(defn visited-guard-steps [{:keys [guard obstacles width height]}]
  (loop [position guard 
         direction [0 -1]
         visited #{}]
         (let [next-pos (next-position position direction)]
           (cond
             (out-of-bounds? position width height) visited
             (contains? obstacles next-pos) (recur position (next-direction direction) visited)
             :else (recur next-pos direction (conj visited next-pos))))))

(def level (parse-level grid))
(def visited-steps (visited-guard-steps level))

(println "Solution 1:" (count visited-steps))

(defn infinite-guard-steps? [{:keys [guard obstacles width height]}]
  (loop [position guard
         direction [0 -1]
         visited #{}]
    (let [next-pos (next-position position direction)]
      (cond
        (out-of-bounds? position width height) false
        (contains? visited [next-pos direction]) true
        (contains? obstacles next-pos) (recur position (next-direction direction) visited)
        :else (recur next-pos direction (conj visited [next-pos direction]))))))

(defn number-obstructing-positions [{:keys [guard] :as level}]
  (let [visited-steps (visited-guard-steps level)]
    (reduce + (map (fn [step]
                     (if (infinite-guard-steps?
                          (update level :obstacles #(conj % step)))
                       1 0))
                   (disj visited-steps guard)))))

(println "Solution 2:" (number-obstructing-positions level))