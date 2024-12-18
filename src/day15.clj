(ns day15 (:require [clojure.string :as str]))

(def input "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
")

(defn parse-move [move]
  (case move
    \^ [0 -1]
    \< [-1 0]
    \> [1 0]
    \v [0 1]
    [0 0]))

(defn parse-level [level]
  (let [lines (str/split-lines level)
        height (count lines)
        width (count (first lines))]
    (loop [y 0, x 0, cells [], pos nil]
      (println y x (get-in lines [y x]))
      (cond (>= y height) {:cells cells, 
                           :width width, 
                           :height height,
                           :pos pos}
            (>= x width) (recur (inc y) 0 cells pos)
            :else (case (get-in lines [y x])
                    \# (recur y (inc x) (conj cells :wall) pos)
                    \O (recur y (inc x) (conj cells :crate) pos)
                    \@ (recur y (inc x) (conj cells :void) [x y])
                    (recur y (inc x) (conj cells :void) pos))))))

(defn level-cell [level [x y]]
  ((:cells level) (+ x (* y (:width level)))))

(def parts (str/split input #"\n\n"))
(def level-txt (first parts))
(def moves-txt (str/replace (second parts) "\n" ""))
(def moves (mapv parse-move moves-txt))
(def level (parse-level level-txt))
(defn move-pos [level move]
  level)