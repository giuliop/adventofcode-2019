(def width 25)
(def height 6)
(def pixel-per-layer (* width height))

(defn char-to-int [c]
  (- (int c) 48))

(defn layers []
  (->> (slurp "input_day8")
       (map char-to-int)
       (drop-last)  ; lose newline
       (partition-all pixel-per-layer)))

(defn count-n [n coll]
  (count (filter (partial = n) coll)))

(defn answer []
  (let [layer (apply min-key (partial count-n 0) (layers))]
    (* (count-n 1 layer) (count-n 2 layer))))

