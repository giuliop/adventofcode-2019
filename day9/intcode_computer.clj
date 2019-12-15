(ns intcode-computer
  (:require [clojure.string :as str])
  (:use clojure.test))

(def memory-size 100000) ; # of memory-cells

(defn init [memory]
  "Inits all memory cells beyond the loaded program and up to
  memory-size to zeros"
  (apply conj memory (repeat (- memory-size (count memory)) 0)))

(defn opcode-and-params [memory pc base]
  "Reads instruction at 'pc' in 'memory' and return the opcode and
  paramenters checking the parameters' modes and 'base' and returns
  [opcode param1 param2 param3]"
  (let [n (nth memory pc)
        opcode (rem n 100)
        m1 (rem (quot n 100) 10)
        m2 (rem (quot n 1000) 10)
        m3 (rem (quot n 10000) 10)
        p1-abs (get memory (+ 1 pc))
        p2-abs (get memory (+ 2 pc))
        p3-abs (get memory (+ 3 pc))
        p1-rel (+ p1-abs (if (= 2 m1) base 0))
        p2-rel (+ p2-abs (if (= 2 m2) base 0))
        p3-rel (+ p3-abs (if (= 2 m3) base 0))
        p1 (cond (or (nil? p1-abs) (= 1 m1)) p1-abs
                 (contains? #{3} opcode) p1-rel
                 :else (get memory p1-rel))
        p2 (cond (or (nil? p2-abs) (= 1 m2)) p2-abs
                 (contains? #{} opcode) p2-rel
                 :else (get memory p2-rel))
        p3 p3-rel]
        ;p3 (cond (or (nil? p3-abs) (= 1 m3)) p3-abs
                 ;(contains? #{} opcode) p3-rel
                 ;:else (get memory p3-rel))]
    [opcode p1 p2 p3]))

(defn run
  ([input memory]
  "Run with default value pc = 0 and end? = true"
   (run input memory 0 true))

  ([input memory pc end?]
  "Run the program in memory starting at program counter pc with
  the given input list and if mode? return when program halts, otherwise
  when it emits output with the following value:
  [[outputs] [memory] [inputs] program-counter base ended?]"
   (loop [pc pc
          base 0
          memory (init memory)
          input input
          output []]
     (let [[opcode p1 p2 p3] (opcode-and-params memory pc base)]
       ;(println opcode p1 p2 p3 " -> " pc base input output)
       ;(println)
       (case opcode
         99 [output memory input pc base true]
         ; 3 read an input and store it at address p1
         3 (recur (+ pc 2) base
                  (assoc memory p1 (first input))
                  (rest input)
                  output)
         ; 4 write an output
         4 (let [output (conj output p1)]
             (if end? (recur (+ pc 2) base memory input output)
               [output memory input (+ pc 2) base false]))
         ; 1 addition ; 2 multiplication
         (1 2) (let [op (if (= 1 opcode) + *)]
                 (recur (+ pc 4) base
                        (assoc memory p3 (op p1 p2))
                        input output))
         ; 5 jump-if-true , 6 jump-if-false
         (5 6) (let [op (if (= 5 opcode) not= =)]
                 (recur (if (op p1 0) p2 (+ 3 pc))
                        base memory input output))
         ; 7 less than ; 8 equals
         (7 8) (let [op (if (= 7 opcode) < =)]
                 (recur (+ pc 4) base
                        (assoc memory p3 (if (op p1 p2) 1 0))
                        input output))
         ; 9 adjust the base by p1
         9 (recur (+ pc 2) (+ base p1) memory input output))))))

; TESTING
(deftest t1
  (is (= [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]
         (first (run [] [109 1 204 -1 1001 100 1 100 1008 100 16 101
                         1006 101 0 99])))))

(deftest t2
  (is (= 16 ((comp count str first first)
         (run [] [1102 34915192 34915192 7 4 7 99 0])))))

(deftest t3
  (is (= [1125899906842624]
         (first (run [] [104 1125899906842624 99])))))

(deftest t4
  (is (= [-1] (first (run [] [109, -1, 4, 1, 99]))))
  (is (= [1] (first (run [] [109, -1, 104, 1, 99]))))
  (is (= [109] (first (run [] [109, -1, 204, 1, 99]))))
  (is (= [204] (first (run [] [109, 1, 9, 2, 204, -6, 99]))))
  (is (= [204] (first (run [] [109, 1, 109, 9, 204, -6, 99]))))
  (is (= [204] (first (run [] [109, 1, 209, -1, 204, -106, 99]))))
  (is (= [100] (first (run [100] [109, 1, 3, 3, 204, 2, 99]))))
  (is (= [1000] (first (run [1000] [109, 1, 203, 2, 204, 2, 99])))))
