(ns day12 (:require [clojure.string :as str]))

(def input (slurp "input/day12.txt"))

(def lines (str/split-lines input))

(def deltas [[0 -1] [0 1] [-1 0] [1 0]])

(defn adding-position [[py px]]
  (fn [[dy dx]] [(+ py dy) (+ px dx)]))

(defn empty-plot [graph y x]
  {:letter (get-in graph [y x])
   :elements #{}
   :perimeter 0})

(defn calc-plots [graph]
  (let [height (count graph)
        width (count (first graph))]
    (letfn
     [(valid-position? [[py px]]
                       (and (>= px 0) (< px width)
                            (>= py 0) (< py height)))
      (same-letter-as? [pos]
                       (fn [other]
                         (= (get-in graph pos)
                            (get-in graph other))))
      (perimeter [pos]
                 (transduce (comp (map (adding-position pos))
                                  (filter (complement (same-letter-as? pos)))
                                  (map (constantly 1)))
                            + 0
                            deltas))]
      (loop [y 0, x 0
             current-plot (empty-plot graph y x)
             plots []
             frontier [[y x]]
             explored #{}
             added #{}]
        (if (empty? frontier)
          (cond (>= y height) plots
                (>= x width) (recur (inc y) 0
                                    (empty-plot graph (inc y) 0)
                                    plots 
                                    [[(inc y) 0]] explored added)
                :else (recur y (inc x)
                             (empty-plot graph y (inc x))
                             (if (contains? added [y x]) 
                               plots 
                               (conj plots (assoc current-plot 
                                                  :perimeter
                                                  (transduce (map perimeter) 
                                                             + 0 
                                                             (:elements current-plot)))))
                             [[y (inc x)]] explored (into added (:elements current-plot))))
          (let [[pos & next-frontier] frontier
                letter (get-in graph pos)
                neighbors (into [] (comp (map (adding-position pos))
                                         (filter valid-position?)
                                         (filter (same-letter-as? pos))
                                         (filter #(not (contains? explored %))))
                                deltas)]
            (cond (= letter (:letter current-plot))
                  (recur y x (update current-plot :elements conj pos)
                         plots
                         (into next-frontier neighbors)
                         (conj explored pos)
                         added)
                  :else (recur y x current-plot
                               plots
                               next-frontier
                               (conj explored pos)
                               added))))))))

(defn plot-fence-cost [plot]
  (* (count (:elements plot))
     (:perimeter plot)))

(println "Solution 1:"
         (transduce (map plot-fence-cost)
                    + 0
                    (calc-plots lines)))