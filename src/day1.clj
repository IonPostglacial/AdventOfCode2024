(ns day1 
  (:require
   [clojure.string :as str]
   [clojure.java.io :refer [reader]]))

(defn transpose [m]
  (apply mapv vector m))

(defn distance [a b]
  (abs (- a b)))

(def input-lists
  (transpose 
   (into []
         (comp
          (map #(str/split % #"\s+"))
          (map (fn [ab] [(parse-long (get ab 0))
                         (parse-long (get ab 1))])))
         (line-seq (reader "input/day1.txt")))))

(def solution1 
  (reduce + (apply map distance 
                   (into [] (map sort) input-lists))))

(println "solution 1:" solution1)

(def freq-in-2nd (frequencies (get input-lists 1)))

(defn similarity-score [n] (* n (or (freq-in-2nd n) 0)))

(def solution2
  (transduce (comp (map similarity-score)
                   (filter number?)) + 0 (get input-lists 0)))

(println "solution 2:" solution2)