(ns day14 (:require [ clojure.string :as str]))

(def input (slurp "input/day14.txt"))
(def lines (str/split-lines input))
(def height 103)
(def width 101)
(def line-re #"p=([^,]+),([^,]+)\s+v=([^,]+),([^,]+)")
(defn parse-line [line]
  (let [matches (re-matches line-re line)]
    {:p [(parse-long (matches 1)) (parse-long (matches 2))] 
     :v [(parse-long (matches 3)) (parse-long (matches 4))] }))
(defn move-during [t] 
  (fn [{:keys [p v]}]
    (let [[px py] p, [vx vy] v]
      [(mod (+ px (* vx t)) width)
       (mod (+ py (* vy t)) height)])))
(def robots (mapv parse-line lines))

(defn quadrant [[px py]]
  (let [hw (quot width 2) 
        hh (quot height 2)]
    (cond (< px hw) (cond (< py hh) 1
                           (= py hh) 0
                           (> py hh) 3)
          (= px hw) 0
          (> px hw) (cond (< py hh) 2
                          (= py hh) 0
                          (> py hh) 4))))

(def final-positions (mapv (move-during 100) robots))
(def freq-quadrants (frequencies (map quadrant final-positions)))

(println "Solution 1:"
         (* (freq-quadrants 1) 
            (freq-quadrants 2) 
            (freq-quadrants 3) 
            (freq-quadrants 4)))
