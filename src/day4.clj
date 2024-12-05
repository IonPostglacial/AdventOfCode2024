(ns day4
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "input/day4.txt")))
(defn match-xmas [ss] (if (re-find #"(XMAS|SAMX)" (apply str ss)) 1 0))
(defn transpose [a] (apply mapv vector a))
(defn tblr-diagonal [grid]
  (into [] (for [y (range (- (count grid) 3))
                 x (range (- (count (first grid)) 3))]
             (into [] (for [i (range 4)]
                        (get-in grid [(+ y i) (+ x i)]))))))
(println "Solution 1:"
         (reduce + (map match-xmas
                        (mapcat #(partition 4 1 %)
                                (-> input
                                    (into (transpose input))
                                    (into (tblr-diagonal input))
                                    (into (tblr-diagonal
                                           (transpose (mapv str/reverse input)))))))))
(defn match-x-mas [ss] (if (re-find #"(M.S.A.M.S|S.S.A.M.M|S.M.A.S.M|M.M.A.S.S)"
                                    (apply str ss)) 1 0))
(defn grid-window [grid]
  (into [] (for [y (range (- (count grid) 2))
                 x (range (- (count (first grid)) 2))]
             (into [] (for [j (range 3) i (range 3)]
                        (get-in grid [(+ y j) (+ x i)]))))))
(println "Solution 2:"
         (reduce + (map match-x-mas (grid-window input))))