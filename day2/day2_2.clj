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

(defn memory [noun verb]
  "Return the input memory"
  (-> (map #(. Integer parseInt %)
           (drop-last (str/split (slurp "input_day2") #",")))
      (vec)
      (assoc 1 noun)
      (assoc 2 verb)))

(defn answer []
  "Return the quiz answer"
  (for [noun (range 100) verb (range 100)
        :let [output (first (run (memory noun verb)))]
        :when (= output 19690720)]
    (-> 100 (* noun) (+ verb))))
