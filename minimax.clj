(ns minimax
  (:require [utils :as utils]))

(defn eval-board
  [board]
  (if-let [row (some (fn [row] (if (apply = row) row nil)) board)]
    (if (= (first row) "O") 10 -10)
    (if-let [row (some (fn [row] (if (apply = row) row nil))
                       (apply mapv vector board))]
      (if (= (first row) "O") 10 -10)
      (if-let [row (when (apply = (map-indexed #(get %2 %1) board))
                     (first board))]
        (if (= (first row) "O") 10 -10)
        (if-let [row (when (apply =
                             (map-indexed ((fn []
                                             (let [c (count (first board))]
                                               (fn [idx itm]
                                                 (get itm (- c idx))))))
                                          board))
                       (first board))]
          (if (= (last row) "O") 10 -10)
          0)))))

(defn minimax
  [board d x]
  (let [s (eval-board board)]
    (cond (not= 0 s) s
          (not (utils/empty-place-exists board)) 0
          :else
            (if-let [pos (utils/find2d board utils/empty-place?)]
              (minimax (assoc-in board pos x) (inc d) (utils/switch-player x))
              (if (= x "O") -1000 1000)))))

(defn best-move
  [board d x]
  (if-let [idx (first (reduce (fn [[bm bs] [idx val]]
                                (if (utils/empty-place? val)
                                  (if-let [s (minimax (assoc-in board
                                                        ((fn [cols]
                                                           (vector
                                                             (quot idx cols)
                                                             (rem idx cols)))
                                                          (count (first board)))
                                                        x)
                                                      d
                                                      (utils/switch-player x))]
                                    (if (> s bs) [idx s] [bm bs])
                                    [bm bs])
                                  [bm bs]))
                        [nil -1000]
                        (map-indexed vector (flatten board))))]
    (#(vector (quot idx %) (rem idx %)) (count (first board)))
    nil))
