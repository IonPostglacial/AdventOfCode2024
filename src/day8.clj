(ns day8 (:require [clojure.string :as str]))

(def input (slurp "input/day8.txt"))

(def lines (str/split-lines input))
(def height (count lines))
(def width (count (first lines)))

(defn assocat [acc2 [k v]]
  (update acc2 k #(concat % v)))

(defn parse-line [idx text]
  (reduce (fn [acc [cidx c]] 
            (if (= c \.) 
              acc 
              (assoc acc c [[cidx idx]]))) {} (map-indexed vector text)))

(defn parse-map [m]
  (reduce (fn [acc1 m]
            (reduce assocat acc1 m)) {} (map-indexed parse-line m)))

(defn- index-pairs
  [cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 3)] (+ j cnt -3)))),
         iter-comb
         (fn iter-comb [c j]
           (if (> j 2) nil
               (let [c (assoc c j (dec (c j)))]
                 (if (< (c j) j) [c (inc j)]
                     (loop [c c, j j]
                       (if (= j 1) [c j]
                           (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
         step
         (fn step [c j]
           (cons (rseq (subvec c 1 3))
                 (lazy-seq (let [next-step (iter-comb c j)]
                             (when next-step (step (next-step 0) (next-step 1)))))))]
     (step c 1))))

(defn pairs [elements]
  (let [v-items (vec (reverse elements)) cnt (count elements)]
    (map #(map v-items %) (index-pairs cnt))))

(defn pair-diff [[p1x p1y] [p2x p2y]]
  [(- p2x p1x) (- p2y p1y)])

(defn pair-antinodes [[p1x p1y :as p1] [p2x p2y :as p2]]
  (let [[dx dy] (pair-diff p1 p2)]
    [[(- p1x dx) (- p1y dy)]
     [(+ p2x dx) (+ p2y dy)]]))

(defn in-bounds? [[px py]]
  (and (>= px 0) (>= py 0) (< px width) (< py height)))

(->> (parse-map lines)
     vals
     (mapcat pairs)
     (mapcat #(apply pair-antinodes %)))

(println "Solution 1:"
         (count (distinct
                 (->> (parse-map lines)
                      vals
                      (mapcat pairs)
                      (mapcat #(apply pair-antinodes %))
                      (filter in-bounds?)))))

(defn pair-harmonics [p1 p2]
  (let [[dx dy] (pair-diff p1 p2)]
    (concat (loop [harmonics [p1 p2] [px py] p1]
              (let [next-p [(- px dx) (- py dy)]]
                (if (in-bounds? next-p)
                  (recur (conj harmonics next-p) next-p)
                  harmonics)))
            (loop [harmonics [] [px py] p1]
              (let [next-p [(+ px dx) (+ py dy)]]
                (if (in-bounds? next-p)
                  (recur (conj harmonics next-p) next-p)
                  harmonics))))))

(println "Solution 2:"
         (count (distinct
                 (->> (parse-map lines)
                      vals
                      (mapcat pairs)
                      (mapcat #(apply pair-harmonics %))
                      (filter in-bounds?)))))