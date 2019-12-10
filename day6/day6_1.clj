(require '[clojure.string :as str])

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

(def filename "input_day6")
;(def filename "input_test")

(defn answer []
  "Return the quiz answer"
  (let [orbit-map (load-input filename)]
  (reduce (fn [res item] (+ res (orbits item orbit-map)))
          0
          (keys orbit-map))))
