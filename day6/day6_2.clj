(require '[clojure.string :as str]
         '[clojure.set :as sets])

(defn load-input [filename]
  "Return the input memory"
  (reduce (fn [coll [x y]] (into coll (hash-map y x)))
          {}
          (map #(str/split % #"\)")
            (str/split-lines (str/trim (slurp filename))))))

(defn orbits [star orbit-map]
  "Return the # of direct and indirect orbits of a star given an
  orbit-map"
  (loop [res 0
         k star]
    (if-let [v (orbit-map k)]
      (recur (inc res) v)
      res)))

(defn orbit-path [star orbit-map]
  "Return a path from the star to the Universal Center of Mass in the
  form of {k v} where k is a star and v the steps from star"
  (loop [path {}
         k star
         steps 0]
    (let [v (orbit-map k)
          path (into path {v steps})]
      (if (= "COM" v) path
        (recur path v (inc steps))))))

(defn closest-orbit [star1 star2 orbit-map]
  "Return the minimum # of steps to move star1 in orbit of star2"
  (let [path-star1 (orbit-path star1 orbit-map)
        path-star2 (orbit-path star2 orbit-map)
        common-stars (sets/intersection (set (keys path-star1))
                                        (set (keys path-star2)))
        path-fun #(+ (path-star1 %) (path-star2 %))]
    (path-fun (apply min-key path-fun common-stars))))

(def filename "input_day6")
;(def filename "input_test")

(defn answer []
  "Return the quiz answer"
  (let [orbit-map (load-input filename)]
    (closest-orbit "YOU" "SAN" orbit-map)))
