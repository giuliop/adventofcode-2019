(require '[intcode-computer :as computer :reload true]
         '[clojure.string :as str])

(defn input-program []
  (vec (map #(BigInteger. %)
            (str/split (str/trim (slurp "input_day11")) #","))))

(def black 0) (def white 1)
(def left 0) (def right 1)
(def north 0) (def east 1) (def south 2) (def west 3)

(defn turn [dir turn-to]
  "Takes a current direction (e.g., :south) and a 90 degree
  left/right turn-to and retursn the new direction"
  (mod (+ dir (if (= right turn-to) right -1)) 4))

(defn move [[x y] dir]
  "Takes a location [x y] and a direction (e.g., north) and return the
  new location by moving of one step in direction"
  (cond (= dir north) [x (dec y)]
        (= dir south) [x (inc y)]
        (= dir east) [(inc x) y]
        (= dir west) [(dec x) y]))

(defn painted [program start-color]
  "Takes a program and returns a set of painted panels as coordinates
  [x y] based on starting robot location as [0 0] and x growing left to
  right, y growing top to bottom"
  (loop [panel [0 0]
         direction north
         input [start-color]
         prog-state [input program 0 0 false]
         painted {} ; map of visited panels with color as value
         ]
    (let [[[new-color] prog-state] (apply computer/run prog-state)
          [[turn-to] prog-state] (apply computer/run prog-state)]
      (if (true? (last prog-state)) painted
        (let [painted (assoc painted panel new-color)
              direction (turn direction turn-to)
              panel (move panel direction)
              input [(if-let [color (painted panel)] color black)]
              prog-state (assoc prog-state 0 input)
              ]
          (recur panel direction input prog-state painted))))))

(defn answer-part-1 []
  (count (keys (painted (input-program) black))))

(defn draw-answer [answer-string]
  (->> (map #(if (= (char (+ white 48)) %) "\u2022" " ") answer-string)
       (clojure.string/join)
       (re-seq #".{1,43}")
       (map println)))

(defn paint-as-string [painted]
  "Takes a map of painted panesl and return a string of black and white
  colors for the panels"
  (let [maxX (apply max (map first (keys painted)))
        minX (apply min (map first (keys painted)))
        maxY (apply max (map second (keys painted)))
        minY (apply min (map second (keys painted)))
        panels (for [y (range minY (inc maxY))
                     x (range minX (inc maxX))]
                 [x y])]
    (apply str (vec (map #(if-let [color (painted %)] color black)
                         panels)))))

(defn answer-part-2 []
  (let [painted (painted (input-program) white)]
    (draw-answer (paint-as-string painted))))

