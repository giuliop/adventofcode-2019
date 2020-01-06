(require '[intcode-computer :as computer :reload true] '[math :reload true]
         '[clojure.set :as sets]
         '[clojure.edn :as edn]
         '[clojure.string :as string])

(def program (->> (string/split (slurp "input_day15") #",")
                  (map edn/read-string)
                  (vec)))

(def north 1)
(def south 2)
(def west 3)
(def east 4)

(defn inverse [dir]
  (condp = dir
    north south
    south north
    east west
    west east))

(def wall 0)
(def space 1)
(def goal 2)
(def start 3)
(def frontier 4)

(defn adjacents [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn adjacents? [pos1 pos2]
  "Returns true if pos1 is adjacent to pos2"
  (some #(= pos1 %) (adjacents pos2)))

(defn add-to-path [[pos {:keys [len path]}]]
  "Takes an entry and add its pos to its path"
  [pos {:len (inc len) :path (conj path pos)}])

(defn adjacents-with-path [[[x y] data]]
  "Return the coordinate of adjacent cells (except diagonals) and path
  to them as [[[x y] {:len n :path [..]}] ...]"
  (->> (adjacents [x y])
       (map #(add-to-path [% data]))
       (vec)))

(defn move [[x1 y1 :as from] [x2 y2 :as to]]
  "Returns the direction (e.g., north) of moving from -> to
  Note that from and to must be different"
  (let [xdir (- x2 x1)
        ydir (- y2 y1)]
    (cond (pos? ydir) south
          (neg? ydir) north
          (pos? xdir) east
          (neg? xdir) west)))

(defn retrace [from to path]
  "Returns the moves [north, south ...] to go from [x y] -> to [x y]
  Assumes from and to are both in path"
  (let [from-index (.indexOf path from)
        to-index (.indexOf path to)
        step (compare to-index from-index)]
    (reduce
      (fn [moves i] (conj moves (move (path i) (path (+ i step)))))
      []
      (range from-index to-index step))))

(defn from-pos-with-dir [[x y] dir]
  "Return the [x' y'] of moving from [x y] with dir"
  (condp = dir
    north [x (dec y)]
    south [x (inc y)]
    east [(inc x) y]
    west [(dec x) y]))

(defn common [from-path to-path]
  "Returns the farthest position in to-path within from-path"
  (loop [to-path to-path]
    (let [pos (last to-path)
          x (some #{pos} from-path)]
      (if x x
        (recur (drop-last to-path))))))

(defn moves [[from from-data] [to to-data]]
  "Returns a list of moves to go from -> to"
  (let [back (common (:path from-data) (:path to-data))]
    (concat (retrace from back (:path from-data))
            (retrace back to (:path to-data)))))

(defn run [[cpos cdata :as current] [epos edata :as entry] state]
  "Takes the current position and a new entry to try; retraces its
  steps if needed and then try entry in the intcode computer.
  Returns output state and the new current-pos
  (which can be different if it backtracked)"
  (loop [dirs (moves current entry)
         state state]
    (let [dir (first dirs)
          dirs (rest dirs)
          [[out] state] (apply computer/run (assoc state 0 [dir]))]
      (if (empty? dirs)
        [out state (from-pos-with-dir epos (inverse dir))]
        (recur dirs state)))))

(defn pop-entry [m]
  "Returns [entry m] where entry is the minimum path entry in m
  and m is without entry"
  (if (empty? m) (throw (Exception. "pop-entry called with empty map"))
    (let [[k v] (apply min-key (fn [[k v]] (:len v)) m)]
      [[k v] (dissoc m k)])))

(defn add-entry [[pos {:keys [len path] :as data}] m]
  "Update the map m adding the entry if new or swapping it for an old
  entry if it has a shorter len"
  (let [current (m pos)]
    (if (or (nil? current) (< len (:len current)))
      (assoc m pos data)
      m)))

(defn add-paths [current to-explore explored]
  "Returns [to-explore explored] updated with new paths from current"
  ;(println current)
  (reduce (fn [[to-explore explored] [pos data :as entry]]
            (if (contains? explored pos)
              [to-explore (add-entry entry explored)]
              [(add-entry entry to-explore) explored]))
          [to-explore explored]
          (adjacents-with-path current)))

(defn add-type [entry atype]
  "Returns the entry with the associated type (wall, space...)"
  [(first entry) (assoc (second entry) :type atype)])

(defn goal-entry [explored]
  "Returns the entry respresenting the goal"
  (first (filter #(= goal (:type (second %))) explored)))

(defn astar
  "Return the minimum path to reach goal as a vector of coordinates"
  ([] (astar false false))
  ([draw? full-map?]
   (loop [current [[0 0] {:len 0 :path [[0 0]] :type start}]
          [to-explore explored] (add-paths current {}
                                           (add-entry current {}))
          state [[] program 0 0 false]]
     (let [[entry to-explore] (pop-entry to-explore)
           [out state current-pos] (run current entry state)
           entry (add-type entry out)
           explored (add-entry entry explored)
           [to-explore explored] (if (= wall out) [to-explore explored]
                                   (add-paths entry to-explore explored))
           current [current-pos (explored current-pos)]
           current (if (= wall out) current entry)]
       (when draw? (do (read-line)(draw explored to-explore)))
       (cond (and (= goal out) (not full-map?)) [entry explored to-explore]
             (empty? to-explore) [(goal-entry explored) explored to-explore]
             ;(empty? to-explore) [(goal explored) explored to-explore]
             :else (recur current [to-explore explored] state))))))

(defn tile-glyph [tile-id]
  (condp = tile-id
    nil "|"
    frontier "?"
    start "O"
    space " "
    wall "\u2588"
    goal "X"))

(defn screen-string [explored]
  (let [minX (apply min (map first (keys explored)))
        minY (apply min (map second (keys explored)))
        maxX (apply max (map first (keys explored)))
        maxY (apply max (map second (keys explored)))
        y-center (max 0 (+ 20 minY))
        x-center (max 0 (+ 20 minX))
        xy (for [y (range minY (inc maxY))
                 x (range minX (inc maxX))]
             [x y])]
    (->> (map tile-glyph (map #(:type (explored %)) xy))
         (partition (inc (- maxX minX)))
         (interpose "\n")
         (apply concat)
         (string/join ))))

(defn draw [explored to-explore]
  (let [to-explore (map #(add-type % frontier) to-explore)
        m (into explored to-explore)]
    (print (str (char 27) "[;H")) ; move cursor top-left
    (println) (println) (println)
    (println (screen-string m)) (println)))

(defn run-and-draw []
  (let [[result explored to-explore] (astar)]
    (print (str (char 27) "[;H")) ; move cursor top-left
    (println) (println result) (println)
    (draw explored to-explore)))

(defn answer-part-1 []
  (:len (second (first (astar)))))

(defn answer-part-2 []
  "Takes a map of the world and calculates how many turns it takes to fill
  it with oxygen"
  (let [[start world _] (astar false true)
        start (first start)]
    (loop [locations (set (map first
                               (filter #(= space (:type (second %))) world)))
           frontier #{start}
           steps 0]
      (if (empty? locations) steps
        (let [frontier (reduce (fn [acc pos]
                                 (into acc (sets/intersection
                                             locations
                                             (set (adjacents pos)))))
                               #{}
                               frontier)
              locations (sets/difference locations frontier)]
          (recur locations frontier (inc steps)))))))
