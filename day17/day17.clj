(require '[intcode-computer :as computer]
         '[clojure.edn :as edn]
         '[clojure.string :as string])
(use 'clojure.test)

(def prog
  (let [p (-> (string/trim (slurp "input_day17"))
              (string/split #","))]
    (vec (map edn/read-string p))))

(def camera-image
 (first (computer/run [] prog)))

(defn draw-camera []
  (println (string/join (map char camera-image))))

(def scaffold (int \#))
(def space (int \.))
(def robot-up (int \^))
(def robot-down (int \v))
(def robot-right (int \>))
(def robot-left (int \<))
(def newl (int \newline))

(def xs (.indexOf camera-image newl))
(def rows (partition xs (inc xs) camera-image))
(def ys (count rows))

(def grid
  (reduce-kv (fn [acc i item]
                 (let [x (rem i xs), y (quot i xs)]
                   (assoc acc [x y] item)))
             {}
             (vec (remove #{newl} camera-image))))

(defn adjacents [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn intersection? [[x y]]
  (and (#{scaffold} (grid [x y]))
       (every? #{scaffold} (map grid (adjacents [x y])))))

(defn answer-part-1 []
  (apply + (map (fn [[x y]] (* x y)) (filter intersection? (keys grid)))))

(defn facing [[x y] dir]
  "Takes robot position and direction and returns the position it is facing"
  (condp = dir
    robot-up [x (dec y)]
    robot-down [x (inc y)]
    robot-right [(inc x) y]
    robot-left [(dec x ) y]))

(defn turn-right [dir]
  (condp = dir
    robot-up robot-right
    robot-right robot-down
    robot-down robot-left
    robot-left robot-up))

(defn turn-to-scaffold [robot dir]
  (let [back-dir (turn-right (turn-right dir))]
    (loop [new-dir (turn-right dir)]
      (if (and (= scaffold (grid (facing robot new-dir)))
               (not= new-dir back-dir))
        new-dir
        (recur (turn-right new-dir))))))

(defn turn [dir new-dir]
  (let [from (condp = dir robot-up 1, robot-right 2, robot-down 3, robot-left 4)
        to (condp = new-dir robot-up 1, robot-right 2, robot-down 3, robot-left 4)
        turn (- to from)]
    (cond (and (= from 4) (= to 1)) "R"
          (and (= from 1) (= to 4)) "L"
          (pos? turn) "R"
          (neg? turn) "L"
          :else nil)))

(defn robot-pos []
  (first (filter
           #(#{robot-up robot-down robot-right robot-left} (grid %)) (keys grid))))

(def path
  (loop [robot (robot-pos)
         dir (grid robot)
         path []
         to-visit (disj (set (filter #(#{scaffold} (grid %)) (keys grid))) robot)]
  (if (empty? to-visit) path
    (let [turn? (not= scaffold (grid (facing robot dir)))
          new-dir (if-not turn? dir (turn-to-scaffold robot dir))
          new-robot (if turn? robot (facing robot dir))
          path (cond turn? (conj path (turn dir new-dir))
                     (number? (peek path)) (update path (dec (count path)) inc)
                     :else (conj path 1))]
      (recur new-robot new-dir path (disj to-visit new-robot))))))

(defn subpaths [s]
  (filter #(<= (count %) 10) (rest (reductions conj [] s))))

(def subpath-1
  (subpaths path))

(def subpath-3
  (reduce (fn [acc i] (conj acc (take-last i path)))
          []
          (range 1 11)))

(defn match-start? [coll subcoll]
  (= subcoll (take (count subcoll) coll)))

(defn remove-paths-from-start [path p1 p2]
  (let [start1 (take (count p1) path)
        start2 (take (count p2) path)]
    (if (and (not= start1 p1) (not= start2 p2)) path
      (let [p (if (= start1 p1) p1 p2)]
        (recur (drop (count p) path) p1 p2)))))

(defn subpath-2 [p1 p3]
  (subpaths (remove-paths-from-start path p1 p3)))

(defn candidates []
  (for [p1 subpath-1, p3 subpath-3, p2 (subpath-2 p1 p3)]
    [p1 p2 p3]))

(defn solution? [[p1 p2 p3]]
  (loop [path path
         sol []]
    (cond (empty? path) (if (<= (count sol) 10) [sol p1 p2 p3] nil)
          (match-start? path p1) (recur (drop (count p1) path) (conj sol "A"))
          (match-start? path p2) (recur (drop (count p2) path) (conj sol "B"))
          (match-start? path p3) (recur (drop (count p3) path) (conj sol "C"))
          :else nil)))

;(def A ["R" 12 "L" 10 "R" 12])
;(def B ["L" 8 "R" 10 "R" 6])
;(def C ["R" 12 "L" 10 "R" 10 "L" 8])

(defn solution []
  (str (apply str (interpose "\n"
                  (map #(apply str (interpose "," %))
                       (solution? (first (filter solution? (candidates)))))))
       "\n"))

(defn video [b]
  (if b "y\n" "n\n"))

(defn answer-part-2 [video?]
  (let [input (map int (str (solution) (video video?)))]
    (last (first (computer/run input (assoc prog 0 2))))))

; TESTING

(defn string-from-grid []
  (->> (for [y (range ys), x (range xs)] [x y])
       (map #(char (grid %)))
       (partition xs)
       (interpose "\n")
       (apply concat)
       (string/join)))

(deftest test-grid
  (is (= (string/join (map char (take (- (count camera-image) 2) camera-image)))
         (string-from-grid))))

