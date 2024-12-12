(ns day11 (:require [clojure.math :refer [log10 pow]]))

(def input (slurp "input/day11.txt"))

(def stone-line (mapv parse-long (re-seq #"\d+" input)))

(defn count-digits [n]
  (long (inc (log10 n))))

(defn next-stones [stone]
  (if (= stone 0) [1]
      (let [len (count-digits stone)]
        (if (even? len)
          (let [p (long (pow 10 (quot len 2)))]
            [(quot stone p)
             (mod stone p)])
          [(* stone 2024)]))))
(count-digits 10)
(next-stones 10)

(defn n-times [n f]
  (apply comp (repeat n f)))

(defn next-stone-line [line]
  (mapcat next-stones line))

(println "Solution 1:"
         (count ((n-times 25 next-stone-line) stone-line)))

; 1000 years later maybe
;(count ((n-times 75 next-stone-line) stone-line))