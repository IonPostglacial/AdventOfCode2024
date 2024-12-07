(ns day7 (:require [clojure.string :as str]))

(def input (slurp "input/day7.txt"))
(def lines (str/split-lines input))

(defn cartesian-product [colls]
  (if (empty? colls)
    '(())
    (for [more (cartesian-product (rest colls))
          x (first colls)]
      (cons x more))))

(defn selections
  [items n]
  (cartesian-product (take n (repeat items))))

(defn parse-equation [equation]
  (let [numbers (mapv parse-long (re-seq #"\d+" equation))]
    [(first numbers) (subvec numbers 1)]))

(def operators [+ *])

(defn apply-operations [operations operands]
  (second (reduce (fn [[operations acc] n] [(rest operations) ((first operations) acc n)])
                  [(cons + operations) 0] operands)))

(defn equation-can-be-solved-with-operators? [operators]
  (fn [equation]
    (let [test-value (first equation) operands (second equation)]
      (some #(= test-value %)
            (map (fn [ops] (apply-operations ops operands))
                 (selections operators (dec (count operands))))))))

(println "Solution 1:" (->> (map parse-equation lines)
                            (filter (equation-can-be-solved-with-operators? operators))
                            (map first)
                            (reduce +)))

(defn cat-num [n m]
  (parse-long (str n m)))

(def operators-3 [+ * cat-num])

(println "Solution 2:" (->> (map parse-equation lines)
                            (filter (equation-can-be-solved-with-operators? operators-3))
                            (map first)
                            (reduce +)))


