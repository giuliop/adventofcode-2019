(require '[math]
         '[clojure.edn :as edn]
         '[clojure.string :as string])
(use 'clojure.test)

(def input (->> (string/trim (slurp "input_day16"))
                  (partition 1)
                  (map (partial apply str))
                  (map edn/read-string)
                  (vec)))

(def base [0 1 0 -1])

(defn pattern [pos]
  "Builds the pattern for position n of the output:
  1) repeat each element pos times
  2) offset the pattern left by one (skip first value)"
  (->> (map (partial repeat pos) base)
       (repeat)
       (flatten)
       (rest)))

(defn FFT-element [n input]
  "Applies FFT to element n of input (where first element is n=0)"
  (->> (map * input (pattern (inc n)))
       (apply +)
       (str)
       (last)
       (str)
       (edn/read-string)))

(defn FFT-phase [input]
  "Applies phase n of FFT to input"
  (map-indexed FFT-element (repeat (count input) input)))

(defn FFT [n input]
  "Applies n phases of FFT to input"
  (nth (iterate FFT-phase input) n))

(defn answer-part-1 []
  (apply str (take 8 (FFT 100 input))))

(defn sum-units
  "Sums digits returning only the unit digit"
  ([a b] (rem (+ a b) 10))
  ([digits] (reduce sum-units 0 digits)))

(defn FFT-element-optmized [n input len]
  "Assume count input is even"
  (let [x (if (> n (/ len 3)) (sum-units (take n input))
            (->> (partition n n [] input)
                 (map-indexed (fn [n input]
                                (if (odd? n) 0
                                  (if (zero? (mod n 4)) (apply + input)
                                    (- (apply + input))))))
                 (apply +)))]
    (math/abs (rem x 10))))

(defn FFT-phase-optimized-first-half [input]
  (let [len (count input)]
    (reduce (fn [out n]
              (conj out (FFT-element-optmized n (drop (dec n) input) len)))
            []
            (range 1 (inc (/ len 2))))))

(defn FFT-phase-optimized-second-half
  ([input] (FFT-phase-optimized-second-half input 1))
  ([input n]
   (let [len (count input)]
     (cond (neg? n) (throw (Exception. "n must >= zero"))
           (zero? n) input
           (= 1 n) (reduce (fn [out n]
                             (conj out (sum-units (first out)
                                                  (nth input (dec n)))))
                           (list (peek input))
                           (range (dec len) 0 -1))
           :else
           (let [input (reduce (fn [out n]
                                 (conj out (sum-units (peek out)
                                                      (nth input (dec n)))))
                               (vector (peek input))
                               (range (dec len) 0 -1))]
             (loop [n (dec n), input input]
               (if (zero? n) (reverse input)
                 (recur (dec n)
                        (reduce (fn [out n]
                                  (conj out (sum-units (nth out (dec n))
                                                       (nth input n))))
                                (vector (first input))
                                (range 1 len))))))))))

(defn FFT-phase-optimized [input]
  "Applies phase n of FFT to input"
  (let [len (count input)]
    (concat
      (FFT-phase-optimized-first-half input)
      (FFT-phase-optimized-second-half (drop (/ len 2) input)))))

(defn FFT-optimized [n input]
  "Applies n phases of FFT to input"
  (nth (iterate FFT-phase-optimized input) n))

(defn FFT-optimized-second-half [n input]
  (FFT-phase-optimized-second-half input n))

(defn answer-part-1-optimized []
  (apply str (take 8 (FFT-optimized 100 input))))

(defn offset [input] (Integer/parseInt (apply str (take 7 input))))

(defn answer-part-2 []
  (let [len (count input)
        len* (* len 10000)
        offset (offset input)
        offset* (- len (rem (- len* offset) len))
        cycles (Math/ceil (/ (- len* offset) len))
        input (vec (take (* len cycles) (cycle input)))
        out (FFT-optimized-second-half 100 input)]
    (apply str (take 8 (drop offset* out)))))

(deftest part2 []
  (let [input [0 3 0 3 6 7 3 2 5 7 7 2 1 2 9 4 4 0 6 3 4 9 1 5 6 5 4 7 4 6 6 4]
        len (count input)
        len* (* len 10000)
        offset (offset input)
        offset* (- len (rem (- len* offset) len))
        cycles (Math/ceil (/ (- len* offset) len))
        input (vec (take (* len cycles) (cycle input)))
        out (FFT-optimized-second-half 100 input)]
    (is (= "84462026" (apply str (take 8 (drop offset* out)))))))

(deftest part1
  (is (= "67481260" (answer-part-1))))

(deftest example
  (let [i [8 0 8 7 1 2 2 4 5 8 5 9 1 4 5 4 6 6 1 9 0 8 3 2 1 8 6 4 5 5 9 5]]
    (is (= "24176176"
           (apply str (take 8 (FFT-optimized 100 i)))))
    (is (= "24176176" (apply str (take 8 (FFT 100 i)))))))
