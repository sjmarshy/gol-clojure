(ns gol.display
  (:gen-class))

(defn get-display-cell
  "return the character used to display the given cell"
  [cell]
  (if (empty? cell)
    \u2591
    \u2588))

(defn get-display-row
  "return a vector of characters used to display a particular row of cells"
  [row]
  (loop [rest-of-row (rest row), current-cell (first row), display []]
    (if (nil? current-cell)
      display
      (recur (rest rest-of-row) (first rest-of-row) (conj display (get-display-cell current-cell))))))

(defn display-row
  "print a row of cells to screen"
  [row]
  (let 
    [r (get-display-row row)]
    (loop [rest-of-r (rest r) cell (first r)]
      (if (nil? cell)
        (println "")
        (do
          (print cell)
          (recur (rest rest-of-r) (first rest-of-r)))))))

(defn display-board
  "print a game-of-life board to screen"
  [board]
  (loop [rest-of-board (rest board), current-row (first board)]
    (if (nil? current-row)
      board
      (do
        (display-row current-row)
        (recur (rest rest-of-board) (first rest-of-board))))))
