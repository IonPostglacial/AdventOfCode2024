(ns day5
  (:require [clojure.string :as str]
            [clojure.set :refer [intersection]]))

(def input (str/split (slurp "input/day5.txt") #"\n\n"))
(def dependencies
  (mapv (fn [dep] (map parse-long (str/split dep #"\|")))
        (str/split-lines (first input))))
(def updates
  (mapv (fn [up] (map parse-long (str/split up #",")))
        (str/split-lines (last input))))
(def dependency-chains (reduce (fn [acc [k v]] (update acc k (fnil conj #{}) v)) {} dependencies))
(defn sorted-update? [up]
  (every? nil? (map-indexed (fn [idx val]
                              (not-empty (intersection
                                          (dependency-chains val)
                                          (set (take idx up)))))
                            up)))

(defn middle-element [v]
  (nth v (quot (count v) 2)))
(println "Solution 1:"
         (transduce (comp (filter sorted-update?)
                          (map middle-element))
                    + updates))

(defn sort-update [up]
  (loop [front [] back up]
    (if-let [head (first back)]
      (if-let [rule (dependency-chains head)]
        (let [unsorted (intersection (set front) rule)]
          (if (empty? unsorted)
            (recur (conj front head) (rest back))
            (recur [] (concat (remove unsorted front) [head] unsorted (rest back)))))
        (recur (conj front head) (rest back)))
      front)))

(println "Solution 2:" (transduce (comp (filter (complement sorted-update?))
                                        (map sort-update)
                                        (map middle-element))
                                  + updates))




