(require '[intcode-computer :as computer :reload true]
         '[math]
         '[clojure.string :as string])

(defn input-program []
  (vec (map #(BigInteger. %)
            (string/split (string/trim (slurp "input_day13")) #","))))

; Every three output instructions specify the x position
; (distance from the left), y position (distance from the top),
; and tile id. The tile id is interpreted as follows
;    0 is an empty tile. No game object appears in this tile.
;    1 is a wall tile. Walls are indestructible barriers.
;    2 is a block tile. Blocks can be broken by the ball.
;    3 is a horizontal paddle tile. The paddle is indestructible.
;    4 is a ball tile. The ball moves diagonally and bounces off objects.
(defn screen
  ([tiles] (screen tiles {}))
  ([tiles current-screen]
   (reduce (fn [screen tile]
             (let [[x y id] tile] (into screen {[x y] id})))
           current-screen
           (partition 3 tiles))))

(defn tiles
  ([program] (tiles program []))
  ([program input] (first (computer/run input program))))

(defn answer-part-1 []
  ((frequencies (vals (screen (tiles (input-program))))) 2))

; Output with X=-1, Y=0, shows the score as tile id
(defn score [scr]
  (int (scr[-1 0])))

(defn tile-glyph [tile-id]
  (case tile-id
    0 " "
    1 "\u2588"
    2 "\u2591"
    3 "\u25AC"
    4 "\u25CF"))

(defn screen-string [screen]
  (let [minX 0 minY 0
        maxX (apply max (map first (keys screen)))
        maxY (apply max (map second (keys screen)))
        xy (for [y (range minY (inc maxY))
                 x (range minX (inc maxX))]
             [x y])]
    (->> (map tile-glyph (map screen xy))
         (partition (inc maxX))
         (interpose "\n")
         (apply concat)
         (string/join))))

(defn draw [screen]
        (println (screen-string screen)))

(defn get- [id scr]
  (first (filter #(= id (scr %)) (keys scr))))

(defn input-ai [scr old-scr]
  "Takes the screen and returns the best move (-1 0 1)"
  (let [[old-ball-x old-ball-y] (get- 4 old-scr)
        [ball-x ball-y] (get- 4 scr)
        [paddle-x paddle-y] (get- 3 scr)

        ball-dir (math/sign (- ball-x old-ball-x))
        gap (- ball-x paddle-x)

        inline? (zero? gap)
        close? (< (- ball-y paddle-y) 2)
        catch-up? (= ball-dir (math/sign gap))
        in-front? (and (> (math/abs gap) 1) (not inline?) (not catch-up?))]

    (cond inline? (if close? 0 ball-dir)
          catch-up? ball-dir
          in-front? (- ball-dir)
          :else 0)))

(defn play [show-game?]
  "Play the game interactively"
  (let [prog (assoc (input-program) 0 2)
        ;input replay]
        input []]
    (loop [[output [input mem pc base ended?]] (computer/run input prog)
           scr (screen output {})
           moves []]
      (let [new-scr (screen output scr)]
        (when show-game?
          (print (str (char 27) "[;H")) ; move cursor top-left
          (println "Score: " (score new-scr) "                 ")
          (draw new-scr) (println))
        (if ended? (score new-scr)
          (let [input [(input-ai new-scr scr)]]
            (recur (computer/run input mem pc base true)
                   new-scr
                   (conj moves (first input)))))))))

(defn answer-part-2 []
  (play false))
