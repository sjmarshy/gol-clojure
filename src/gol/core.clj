(ns gol.core
  (:require [gol.display :as display])
  (:gen-class))

; we may have some fun with this yet. The plan is to create a simple game of
; life.

(def cell [])
; although this has the living symbol inside it, we'll really just be checking
; for empty
(def live-cell [:living])


(def spaceship (make-board
                 90
                 10
                 [
                  [ 1 0 ]
                  [ 2 0 ]
                  [ 3 0 ]
                  [ 4 0 ]
                  [ 0 1 ]
                  [ 4 1 ]
                  [ 4 2 ]
                  [ 0 3 ]
                  [ 3 3 ]]))

(def die-hard (make-board
                90
                10
                [
                 [ 7 1 ]
                 [ 1 2 ]
                 [ 2 2 ]
                 [ 2 3 ]
                 [ 6 3 ]
                 [ 7 3 ]
                 [ 8 3 ]]))

(def glider-gun (make-board
                  150
                  50
                  [
                   [1 5]
                   [1 6]
                   [2 5]
                   [2 6]
                   [11 5]
                   [11 6]
                   [11 7]
                   [12 4]
                   [12 8]
                   [13 3]
                   [13 9]
                   [14 3]
                   [14 9]
                   [15 6]
                   [16 4]
                   [16 8]
                   [17 5]
                   [17 6]
                   [17 7]
                   [18 6]
                   [21 3]
                   [21 4]
                   [21 5]
                   [22 3]
                   [22 4]
                   [22 5]
                   [23 2]
                   [23 6]
                   [25 1]
                   [25 2]
                   [25 6]
                   [25 7]
                   [35 3]
                   [35 4]
                   [36 3]
                   [36 4]]))

(defn make-empty-board
  [w h]
  (let [row (into [] (take w (repeat cell)))]
    (let [empty-board (into [] (take h (repeat row)))]
      empty-board)))

(defn make-board
  [w h co-ordinates]
  (let [empty-board (make-empty-board w h)]
    (loop [fill-cell-co-ordinates (into [] (first co-ordinates)), rest-of (rest co-ordinates), board empty-board]
      (if (empty? fill-cell-co-ordinates)
        board
        (let [[x y] fill-cell-co-ordinates]
          (recur (into [] (first rest-of)) (rest rest-of) (assoc-in board [y x] live-cell)))))))

(defn remove-nilv
  [vect]
  (filterv (complement nil?) vect))

(defn nil-to-empty
  [vect]
  (mapv (fn
         [i]
         (if (nil? i)
           []
           i)) vect))

(defn get-neighbours
  "get a vector of neighbouring cells"
  [board x y]
  (nil-to-empty [
                (get-in board [(- y 1) x]) ; north-of
                (get-in board [y (+ x 1)]) ; east-of
                (get-in board [(+ y 1) x]) ; south-of
                (get-in board [y (- x 1)]) ; west-of
                (get-in board [(- y 1) (+ x 1)]) ; north-east-of
                (get-in board [(+ y 1) (+ x 1)]) ; south-east-of
                (get-in board [(+ y 1) (- x 1)]) ; south-west-of
                (get-in board [(- y 1) (- x 1)])])) ; north-west-of


(defn count-cell-neighbours
  "count how many neighbours a cell has"
  [board x y]
  (let [neighbours (get-neighbours board x y)]
    (loop [rest-of-n (rest neighbours) n (first neighbours) amount 0]
      (if (nil? n)
        amount
        (if (empty? n)
          (recur (rest rest-of-n) (first rest-of-n) amount)
          (recur (rest rest-of-n) (first rest-of-n) (+ amount 1)))))))


(defn annotate-cell-with-neighbours
  "annotate a cell with how many living neighbours it has"
  [board cell x y]
  (let [new-cell (with-meta cell {:neighbours (count-cell-neighbours board x y)})]
    new-cell))

(defn annotate-row-with-neighbours
  "mark how many neighbours each cell in a row has using metadata"
  [board row y]
  (loop [rest-of-row (rest row), current-cell (first row), x 0, annotated-row []]
    (if (nil? current-cell)
      annotated-row
      (recur (rest rest-of-row) (first rest-of-row) (+ x 1) (conj annotated-row (annotate-cell-with-neighbours board current-cell x y))))))

(defn annotate-board-with-neighbours
  "mark how many neighbours each cell has using metadata"
  [board]
  (loop [rest-of-board (rest board) current-row (first board) y 0 annotated-board []]
    (if (nil? current-row)
      annotated-board
      (recur (rest rest-of-board) (first rest-of-board) (+ y 1) (conj annotated-board (annotate-row-with-neighbours board current-row y))))))

(defn get-cell-based-on-neighbours
  [neighbours alive]
  (if alive
    (cond
      ; die
      (> neighbours 3) []
      ; don't die
      (> neighbours 1) [:living]
      ; die
      :else [])
    (cond
      (= neighbours 3) [:living]
      :else [])))

(defn get-cell-based-on-meta
  [cell]
  (get-cell-based-on-neighbours (:neighbours (meta cell)) ((complement empty?) cell)))

(defn produce-row-from-neigbours
  [row]
  (loop [rest-of-row (rest row) current-cell (first row) new-row []]
    (if (nil? current-cell)
      new-row
      (recur (rest rest-of-row) (first rest-of-row) (conj new-row (get-cell-based-on-meta current-cell))))))

; it may seem odd doing the whole loop-through-the-board thing twice but this
; way we don't accidentally change the neighbours figures while modifying the
; board
(defn produce-board-from-neighbours
  [board]
  (loop [rest-of-board (rest board) current-row (first board) new-board []]
    (if (nil? current-row)
      new-board
      (recur (rest rest-of-board) (first rest-of-board) (conj new-board (produce-row-from-neigbours current-row))))))

(defn do-generation
  "process a board a single generation forward"
  [board]
  (produce-board-from-neighbours (annotate-board-with-neighbours board)))

(defn display-x-generation
  [board x timeout]
  (loop [next-x x, next-board board]
    (if (> next-x 0)
      (do
        (println next-x)
        (display/display-board next-board)
        (Thread/sleep timeout)
        (recur (- next-x 1) (do-generation next-board)))
      next-board)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
