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

(defn laser-point [map-data]
  "Return the location [x y] of the laser"
  (let [blind-map (blind-map map-data)]
    (apply max-key #(count (visible-from % blind-map))
           (:asteroids map-data))))

(defn distance [p1 p2]
  "Distance between two points"
  (let [[x1 y1] p1 , [x2 y2] p2]
    (+ (math/abs (- x2 x1)) (math/abs (- y2 y1)))))

(defn quadrant [point laser]
  "Return the quadrant the point is in respect to the laser:
  4  |  1
  -------     where the laser is in the center
  3  |  2                                                  "
  (let [[x1 y1] laser [x2 y2] point
        x (- x2 x1) y (- y2 y1)]
    (cond (pos? x) (if (neg? y) 1 2)
          (neg? x) (if (pos? y) 3 4)
          (zero? x) (if (neg? y) 1 3))))

(defn angle [point laser map-data]
  "Return a proxy of the angle between laser and point,
  measured clockwise, where a larger number means a smaller
  angle (but angles can only be compared in the same 'quadrant'"
  (let [[x1 y1] laser [x2 y2] point]
    (if (= y2 y1) (inc (:X map-data))
      (/ (- x2 x1) (- y2 y1)))))

(defn priority-data [point laser map-data]
  "Returns :distance :quadrant and :angle for the asteroid vs the laser"
  {:distance (distance point laser)
   :quadrant (quadrant point laser)
   :angle (angle point laser map-data)})

(defn compare-asteroids [asteroid-with-priority-map]
  "Takes a map {:asteroid priority-datad} and returns a comparator
  that says a1 comes before a2 if it has higher vaporization priority"
  ;(fn [[a1 a1-data] [a2 a2-data]]
  (fn [a1 a2]
    (let [a1-data (asteroid-with-priority-map a1)
          a2-data (asteroid-with-priority-map a2)
          x (- (:quadrant a1-data) (:quadrant a2-data))
          ang1 (:angle a2-data)
          ang2 (:angle a1-data)
          z (- (:distance a1-data) (:distance a2-data))]
      (cond (not= 0 x) x
            (< ang1 ang2) -1
            (> ang1 ang2) 1
            :else z))))

(defn vaporization-list [laser map-data]
  "Takes map data {:asteroids [[x y] ...] :X #-col :Y #-row and returns
  a sorted map of the asteroids in vaporization order from point as keys
  and their priority-data as values"
  (let [laser (laser-point map-data)
        asteroids (disj (:asteroids map-data) laser)
        asteroids-with-priority
        (zipmap asteroids
                (map #(priority-data % laser map-data) asteroids))
        cmp-f (compare-asteroids asteroids-with-priority)
        thelist (into (sorted-map-by cmp-f) asteroids-with-priority)
        in-line? (fn [p1-data p2-data] ; are points in line from laser?
                   (and (not (nil? p1-data))
                        (zero? (mod (- (:quadrant p1-data)
                                       (:quadrant p2-data)) 4))
                        (= (:angle p1-data) (:angle p2-data))))
        ; now we need to put in subsequent rounds the asteroids that are
        ; covered by another one and are hit after a full circle; we do
        ; that by adding 4 to their :quadrant and then resorting the map
        thelist
        (first (reduce (fn [[acc [last- last-data]] [x x-data]]
                         (if (in-line? last-data x-data)
                           (let [x-data
                                 (assoc x-data :quadrant
                                        (+ 4 (:quadrant last-data)))]
                             [(assoc acc x x-data) [x x-data]])
                           [acc [x x-data]]))
                       [thelist [nil nil]]
                       thelist))]
    (into (sorted-map-by (compare-asteroids thelist)) thelist)))

(defn answer-part-2
  "Returns the answer to part 2 question"
  ([] (answer-part-2 "input_day10"))
  ([input-file]
   (let [map-data (input-map input-file)
         laser (laser-point map-data)
         [x y] (nth (keys (vaporization-list laser map-data)) (dec 200))]
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

(deftest t2
  (let [solution [[nil 26 nil nil nil nil 29 30 31 2 4 nil nil nil 33 nil nil]
                  [23 24 nil nil nil 27 28 nil 1 3 32 6 7 nil nil 9 34]
                  [21 22 nil nil nil 25 nil nil nil 5 nil 8 10 11 12 13 nil]
                  [nil nil 20 nil nil nil nil nil 0 nil nil nil 14 35 36 nil nil]
                  [nil nil 19 nil 18 nil nil nil nil nil 17 nil nil nil nil 16 15]]
        laser [8 3] ; 0 above
        map-data {:X (count (first solution))
                   :Y (count solution)
                   :asteroids
                      (into #{}
                            (->> solution
                                 (map-indexed
                                   (fn [y line]
                                     (keep-indexed
                                       (fn [x itm] (if (nil? itm) nil [x y])) line)))
                                 (apply concat)))}
        sol-list (->> (vec (:asteroids map-data))
                      (sort-by (fn [[x y]] ((solution y) x)))
                      (rest))  ; we drop element 0, the laser
        computed-list (keys (vaporization-list laser map-data))]
    (is (= sol-list computed-list)))
  (is (= 317 (answer-part-2))))
