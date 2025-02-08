(ns main)

(println "Wellcome to Tic Tak Toe game")

(defn game-winner
  [board pc]
  (cond (some (fn [x] (every? #(= % pc) x)) board) true
        (some (fn [x] (every? #(= % pc) x)) (apply mapv vector board)) true
        (every? #(= % pc) (map-indexed #(get %2 %1) board)) true
        (every? #(= % pc) (map-indexed #(get %2 (- 2 %1)) board)) true
        :else false))

(defn empty-place-exists
  [board]
  (some (fn [row] (some #(Character/isDigit %) row)) board))

(defn get-inp
  [board]
  (println "Enter one of the available places number: ")
  (let [inp (read-line)]
    (if (some #(= (str %) inp) (flatten board))
      inp
      (do (println "Invalid input. Try again") (get-inp board)))))

(defn print-board
  [board]
  (doseq [row board] (println (apply str (interpose " | " row)))))

(defn switch-player [p] (if (= p \X) \O \X))

(defn update-board
  [board pos value]
  (let [pos-n (Integer/parseInt pos)
        row (quot (dec pos-n) 3)
        col (rem (dec pos-n) 3)]
    (assoc-in board [row col] value)))

(defn play
  [board player]
  (println)
  (print-board board)
  (let [inp (get-inp board)
        new-board (update-board board inp player)]
    (cond (game-winner new-board player) (do (print-board new-board)
                                             (println
                                               (str "Player " player " wins!")))
          (empty-place-exists new-board) (play new-board (switch-player player))
          :else (do (print-board new-board) (println "It's a draw!")))))

(play [[\1 \2 \3] [\4 \5 \6] [\7 \8 \9]] \X)
