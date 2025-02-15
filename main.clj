(ns main
  (:require [utils :as utils])
  (:require [minimax :as minimax]))

(println "Wellcome to Tic Tak Toe game")

(defn game-winner
  [board pc]
  (cond (some (fn [x] (every? #(= % pc) x)) board) true
        (some (fn [x] (every? #(= % pc) x)) (apply mapv vector board)) true
        (every? #(= % pc) (map-indexed #(get %2 %1) board)) true
        (every? #(= % pc) (map-indexed #(get %2 (- 2 %1)) board)) true
        :else false))

(defn get-inp
  [board]
  (println "Enter one of the available places number: ")
  (let [inp (read-line)]
    (if (and (utils/empty-place? inp) (some #(= % inp) (flatten board)))
      inp
      (do (println "Invalid input. Try again") (get-inp board)))))

(defn print-board
  [board]
  (doseq [row board] (println (apply str (interpose " | " row)))))

(defn play
  [board player play-bot]
  (println)
  (print-board board)
  (let [new-board (if (and (true? play-bot) (= player "O"))
                    (assoc-in board (minimax/best-move board 0 player) player)
                    (let [inp (get-inp board)]
                      (utils/update-board board #(= % inp) player)))]
    (cond (game-winner new-board player) (do (print-board new-board)
                                             (println
                                               (str "Player " player " wins!")))
          (utils/empty-place-exists new-board)
            (play new-board (utils/switch-player player) play-bot)
          :else (do (print-board new-board) (println "It's a draw!")))))

(play [["1" "2" "3"] ["4" "5" "6"] ["7" "8" "9"]] "X" true)
