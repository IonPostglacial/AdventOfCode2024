(ns day2
  (:require
   [clojure.string :as str]
   [clojure.java.io :refer [reader]]))

(defn parse-report [report]
  (into []
        (map parse-long)
        (str/split report #"\s+")))

(def input
  (into []
        (map parse-report)
        (line-seq (reader "input/day2.txt"))))

(defn is-between [a b]
  (fn [n] (and (>= n a) (<= n b))))

(defn is-step-safe [decreasing]
  (if decreasing
    (is-between -3 -1)
    (is-between 1 3)))

(defn variation [report]
  (map - report (drop 1 report)))

(defn is-variation-safe [diff]
  (or (empty? diff)
      (every? (is-step-safe (< (first diff) 0)) diff)))

(defn is-report-safe [report]
  (is-variation-safe (variation report)))

(println "Solution 1:" (count (filter is-report-safe input)))

(defn omit [v n] (into (subvec v 0 n) (subvec v (inc n))))

(defn report-and-potential-fixes [report]
  (concat (seq [report]) (map #(omit report %) (range (count report)))))

(defn is-report-safe-with-fix [report]
  (some is-report-safe (report-and-potential-fixes report)))

(println "Solution 2:" (count (filter is-report-safe-with-fix input)))
