(require '[clojure.string :as str])

(defn run [memory]
  "Run the program in memory"
  (loop [pos 0
         memory memory]
    (let [opcode (nth memory pos)]
      (if (= 99 opcode) memory
        (let [op (if (= 1 opcode) + *)
              d1 (nth memory (nth memory (+ 1 pos)))
              d2 (nth memory (nth memory (+ 2 pos)))]
          (recur (+ pos 4)
                 (let [result (op d1 d2)
                       result-pos (nth memory (+ pos 3))]
                   (assoc memory result-pos result))))))))

(defn memory []
  "Return the input memory"
  (-> (map #(. Integer parseInt %)
           (drop-last (str/split (slurp "input_day2") #",")))
      (vec)
      (assoc 1 12)
      (assoc 2 2)))

(defn answer []
  "Return the quiz answer"
  (run (memory)))
