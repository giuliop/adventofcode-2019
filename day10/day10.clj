(require '[clojure.string :as string]
         '[math])
(use 'clojure.test)

(defn input-map [input-file]
  "Returns a dictionary with three keys:
  :asteroids -> a set as [x y] where an asteroid is present
  :X the map max x coordinate
  :Y the map max y cooridnate"
  (let [the-map (->> (slurp input-file)
                        (string/split-lines)
                        (map vec))]
    {:X (count (first the-map))
     :Y (count the-map)
     :asteroids
     (into #{}
           (->> the-map
                (map-indexed
                  (fn [y line]
                    (keep-indexed
                      (fn [x itm] (if (= \# itm) [x y] nil)) line)))
                (apply concat)))}))

(defn steps [p1 p2]
  "Returns the steps [x y] to take to move from p2 to all points
  whose visibility p2 blocks to p1"
  (let [[x1 y1] p1
        [x2 y2] p2
        x-step (- x2 x1)
        y-step (- y2 y1)
        step-gcd (math/gcd x-step y-step)
        x-step (if (zero? step-gcd) x-step (/ x-step step-gcd))
        y-step (if (zero? step-gcd) y-step (/ y-step step-gcd))]
    (cond (= p1 p2) [0 0]
          (= x1 x2) [0 (if (> y2 y1) 1 -1)]
          (= y1 y2) [(if (> x2 x1) 1 -1) 0]
          :else [x-step y-step])))

(defn blind-spots [point obstacle map-data]
  "Takes coordinates [x y] of a point and an obstacle and
  generates a list of coordinates of blind spots"
  (let [[x2 y2] obstacle
        [x-step y-step] (steps point obstacle)]
    (if (and (zero? x-step) (zero? y-step)) [point]
      (loop [res []
             x (+ x2 x-step)
             y (+ y2 y-step)]
      (if (or (> x (:X map-data)) (> y (:Y map-data))
              (< x 0) (< y 0)) res
          (recur (conj res [x y])
                 (+ x x-step)
                 (+ y y-step)))))))

(defn blind-map [map-data]
  "Returns a map that associates to each asteroid its blind spots"
  (reduce (fn [acc [point blind-points]]
            (update acc point #(into (if % % #{}) blind-points)))
          {}
          (for [x (:asteroids map-data)
                y (:asteroids map-data)]
            [x (blind-spots x y map-data)])))

(defn visible-from [point blind-map]
  "Returns the asteroids visible from a point given the blind map"
  (let [blinds (blind-map point)]
    (remove #(contains? blinds %) (keys blind-map))))

(defn invisible-from [point blind-map]
  "Returns the asteroids invisible from a point given the blind map"
  (let [blinds (blind-map point)]
    (filter #(contains? blinds %) (keys blind-map))))

(defn answer-part-1
  "Returns maximum # of visible asteroids from any location"
  ([] (answer-part-1 "input_day10"))
  ([input-file]
   (let [map-data (input-map input-file)
         blind-map (blind-map map-data)]
     (apply max (map #(count (visible-from % blind-map))
                     (:asteroids map-data))))))

(defn vaporization-list [point map-data]
  "Takes map data {:asteroids [[x y] ...] :X #-col :Y #-row and returns
  a vector of the asteroinds in vaporization order from point"
)


(defn answer-part-2
  "Returns the answer to part 2 question"
  ([] (answer-part-2 "input_day10"))
  ([input-file]
   (let [map-data (input-map input-file)
         [x y] (nth (vaporization-list) (dec 200))]
     (+ (* 100 x) y))))

;;; TESTING

(deftest t1
  (is (= 8 (answer-part-1 "input_test_8")))
  (is (= 33 (answer-part-1 "input_test_33")))
  (is (= 35 (answer-part-1 "input_test_35")))
  (is (= 41 (answer-part-1 "input_test_41")))
  (is (= 210 (answer-part-1 "input_test_210")))
  (is (= 292 (answer-part-1 "input_day10")))
)
