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

(defn color [& pixels]
  "Takes a series of pixels (first top layer, last bottom layer) and
  returns which color is visible (i.e., first non-trasparent one)
  Colors are: 0 -> black, 1 -> white, 2 -> transparent"
  (loop [color (first pixels)
         pixels (rest pixels)]
    (if (or (not= 2 color) (empty? pixels)) color
      (recur (first pixels) (rest pixels)))))

(defn test-layers [] '((0 2 2 2) (1 1 2 2) (2 2 1 2))) ; (0 0 0 0)))

(defn answer-string []
  ((apply map color (layers))))

(defn draw-answer []
  (->> (map #(if (= 1 %) "\u2022" " ") (answer-string))
       (clojure.string/join)
       (re-seq #".{1,25}")
       (map println)))
