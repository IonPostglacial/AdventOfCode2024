(ns day3)

(def input (slurp "input/day3.txt"))

(defn parse-op [[_ operation & args]]
  (case operation 
    "mul" [* (parse-long (first args)) (parse-long (last args))]
    "do" [:do]
    "don't" [:dont]))

(defn exec-op-1 [[operation op1 op2]]
  (operation op1 op2))

(def sol-1 (comp (map parse-op)
                 (map exec-op-1)))

(println "Solution 1:" 
         (transduce sol-1 + (re-seq #"(mul)[(](\d+)?(:?,(\d+))?[)]" input)))

(defn exec-op-2 [[enabled result] [operation & args]]
  (case operation
    :do [true result]
    :dont [false result]
    [enabled (+ result (if enabled
                         (operation (first args) (last args))
                         0))]))

(println "Solution 2:"
         (second
          (transduce (map parse-op)
                     (completing exec-op-2)
                     [true 0]
                     (re-seq #"(mul|do|don't)[(](\d+)?(:?,(\d+))?[)]" input))))

