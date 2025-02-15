(ns utils)

(defmacro dbg
  ([x]
   `(let [x# ~x]
      (println "dbg:" '~x "=" x#)
      x#))
  ([label x]
   `(let [x# ~x]
      (println "dbg:" ~label "=" x#)
      x#)))

(defn empty-place? [p] (re-matches #"\d+" p))

(defn empty-place-exists
  [board]
  (some (fn [row] (some empty-place? row)) board))

(defn find2d
  [data f & {:keys [from-end all], :or {from-end false, all false}}]
  (let [idxs (keep-indexed (fn [i v] (when (f v) i)) (flatten data))
        cols (count (first data))]
    (cond (zero? cols) nil
          (true? from-end) (let [p (last idxs)]
                             (vector (quot p cols) (rem p cols)))
          (true? all) (map #(vector (quot % cols) (rem % cols)) idxs)
          :else (when-let [p (first idxs)]
                  (vector (quot p cols) (rem p cols))))))

(defn switch-player [p] (if (= p "X") "O" "X"))

(defn update-board
  [board f v]
  (let [pos (find2d board f)
        row (first pos)
        col (second pos)]
    (assoc-in board [row col] v)))
