(require '[clojure.string :as str]
         '[clojure.set :as sets])

; coordinates are expressed as [x,y] where [0,0] is the wire origin,
; up and right are +1 while down and left are -1

(defn move [move-string]
  "Takes a move-string, e.g., 'u123' and splits it in [\\u 123]"
  [ (first move-string) (. Integer parseInt (subs move-string 1)) ])

(defn inputs []
  "Return the two wires movements as two seqs [[\\R 10] ...]"
  (->> (slurp "input_day3")
       (str/split-lines)
       (map #(str/split % #","))
       (map #(map move %))))

(defn nop [x1 x2]
  x1)

(defn endpoint [startpoint move]
  "Takes a startpoint [x,y] and a move [\\R 3] and returns the endoint"
  (let [[x y] startpoint
        [dir steps] move]
    (case dir
      \U [x (+ y steps)]
      \D [x (- y steps)]
      \R [(+ x steps) y]
      \L [(- x steps) y])))

(defn segments [moves]
  "Takes a seq of wire moves and return a vector of wire segments indicated
  by the starting and end coordinate of the segment"
  (reduce
    (fn [val move]
      (let [start (second (last val))
            end (endpoint start move)]
        (conj val [start end])))
    [[[0,0] (endpoint [0,0] (first moves))]]
    (rest moves)))

(defn points [segment]
  "Takes a segment [[x,y] [x2 y2]] and returns a set of points touched"
  (let [[[x1 y1] [x2 y2]] segment
        points #{[x1 y1] [x2 y2]}]
    (reduce conj points
            (if (= x1 x2)
              (for [y (if (> y2 y1) (range y1 y2) (range y2 y1))] [x1 y])
              (for [x (if (> x2 x1) (range x1 x2) (range x2 x1))] [x y1])))))

(defn intersections [moves1 moves2]
  "Takes two seqs of wires moves and return the set of intersections
  excluding [0 0]"
  (let [s1 (reduce sets/union #{} (map points (segments moves1)))
        s2 (reduce sets/union #{} (map points (segments moves2)))]
    (disj (sets/intersection s1 s2) [0 0])))

(defn distance [p1 p2]
  "Returns the manhattan distance of two points [x y]"
  (let [[x1 y1] p1
        [x2 y2] p2]
    (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))

(defn answer []
  "Return the quiz answer"
  (apply min (map #(distance % [0 0]) (apply intersections (inputs)))))


