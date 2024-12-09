(ns day9 (:require [clojure.string :as str]))

(def input (slurp "input/day9.txt"))
(defn parse-file-map [file-map]
  (into [] (comp (map parse-long)
                 (partition-all 2)
                 (map-indexed (fn [id [fsize vsize]]
                                (concat (repeat fsize id)
                                        (if (nil? vsize)
                                          []
                                          (repeat vsize ".")))
                                ))
                 cat)
        (str/split file-map #"")))

(defn compact-blocs [blocs]
  (loop [cur-blocs blocs 
         start 0 
         end (dec (count blocs))]
    (if (<= end start)
      cur-blocs
      (cond (not= (blocs start) ".") (recur cur-blocs (inc start) end)
            (= (blocs end) ".") (recur cur-blocs start (dec end))
            :else (recur (assoc cur-blocs start (cur-blocs end)
                                end (cur-blocs start)) (inc start) (dec end))))))
(println "Solution 2:"
         (transduce (map-indexed (fn [idx fid] (if (= fid ".")
                                                 0
                                                 (* idx fid))))
                    + 0 
                    (compact-blocs (parse-file-map input))))

