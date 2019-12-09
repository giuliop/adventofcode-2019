(require '[clojure.string :as str])

(defn opcode-and-params [n]
  "Read a value and return the opcode and parameter modes as
  [opcode mode-param1 mode-param2 mode-param3]"
  (let [opcode (rem n 100)
        m1 (rem (quot n 100) 10)
        m2 (rem (quot n 1000) 10)
        m3 (rem (quot n 10000) 10)]
    [opcode m1 m2 m3]))

(defn run [input memory]
  "Run the program in memory with the given input list and return
  [ [output values] [memory values] ]"
  (loop [pc 0
         memory memory
         input input
         output []]
    (let [[opcode m1 m2 m3] (opcode-and-params (nth memory pc))]
      (case opcode
        99 [output memory]
        ; 3 read an input
        3 (recur (+ pc 2)
                 (assoc memory (nth memory (+ 1 pc)) (first input))
                 (rest input)
                 output)
        ; 4 write an output
        4 (recur (+ pc 2)
                 memory
                 input
                 (conj output (nth memory (nth memory (+ 1 pc)))))
        ; 1 addition ; 2 multiplication
        (1 2) (let [op (if (= 1 opcode) + *)
                    p1 (nth memory (+ 1 pc))
                    p2 (nth memory (+ 2 pc))
                    res-pos (nth memory (+ 3 pc))
                    d1 (if (= 1 m1) p1 (nth memory p1))
                    d2 (if (= 1 m2) p2 (nth memory p2))]
                (recur (+ pc 4)
                       (assoc memory res-pos (op d1 d2))
                       input output))))))

(defn memory []
  "Return the input memory"
  (vec (map #(. Integer parseInt %)
            (str/split (str/trim (slurp "input_day5")) #","))))

(defn answer []
  "Return the quiz answer"
  (first (run [1] (memory))))

