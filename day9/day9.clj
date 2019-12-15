(require '[intcode-computer :as computer :reload true]
         '[clojure.string :as str])

(defn input-program []
  (vec (map #(. Integer parseInt %)
            (str/split (str/trim (slurp "input_day9")) #","))))

(defn answer-part1 []
  (first (computer/run [1] (input-program))))

(defn answer-part2 []
  (first (computer/run [2] (input-program))))
