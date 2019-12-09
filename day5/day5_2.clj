(require '[clojure.string :as str])

(defn opcode-and-params [memory pc]
  "Read a value and return the opcode and parameter modes as
  [opcode mode-param1 mode-param2 mode-param3]"
  (let [n (nth memory pc)
        opcode (rem n 100)
        m1 (rem (quot n 100) 10)
        m2 (rem (quot n 1000) 10)
        m3 (rem (quot n 10000) 10)
        p1 (get memory (+ 1 pc))
        p2 (get memory (+ 2 pc))
        p3 (get memory (+ 3 pc))
        p1 (if (= 1 m1) p1 (get memory p1))
        p2 (if (= 1 m2) p2 (get memory p2))]
    [opcode p1 p2 p3]))

(defn run [input memory]
  "Run the program in memory with the given input list and return
  [ [output values] [memory values] ]"
  (loop [pc 0
         memory memory
         input input
         output []]
    (let [[opcode p1 p2 p3] (opcode-and-params memory pc)]
      ;(println opcode output memory)
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
        (1 2) (let [op (if (= 1 opcode) + *)]
                (recur (+ pc 4)
                       (assoc memory p3 (op p1 p2))
                       input output))
        ; 5 jump-if-true , 6 jump-if-false
        (5 6) (let [op (if (= 5 opcode) not= =)]
                (recur (if (op p1 0) p2 (+ 3 pc))
                       memory input output))
        ; 7 less than ; 8 equals
        (7 8) (let [op (if (= 7 opcode) < =)]
                (recur (+ pc 4)
                       (assoc memory p3 (if (op p1 p2) 1 0))
                       input output))))))

(defn input-program []
  "Return the input memory"
  (vec (map #(. Integer parseInt %)
            (str/split (str/trim (slurp "input_day5")) #","))))

(defn answer []
  "Return the quiz answer"
  (first (run [5] (input-program))))

