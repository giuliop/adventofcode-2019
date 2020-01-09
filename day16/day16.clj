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

(defn sum-units [digits]
  (reduce (fn [acc n] (rem (+ acc n) 10)) 0 digits))

(defn FFT-element-optmized [n input]
  "Assume count input is even"
  (let [len (count input)
        x (drop (dec n) input)
        x (cond (> n (/ len 2)) (sum-units x)
                (> n (/ len 3)) (sum-units (take n x))
                :else (->> (partition n n [] x)
                           (map-indexed (fn [n input]
                                          (if (odd? n) 0
                                            (if (zero? (mod n 4)) (apply + input)
                                              (- (apply + input))))))
                           (apply +)))]
    (math/abs (rem x 10))))

(defn FFT-phase-optimized [input]
  "Applies phase n of FFT to input"
  (let [f (fn [n] (FFT-element-optmized n input))]
    (pmap f (range 1 (inc (count input))))))

(defn FFT-optimized [n input]
  "Applies n phases of FFT to input"
  (nth (iterate FFT-phase-optimized input) n))

(defn answer-part-1 []
  (apply str (take 8 (FFT-optimized 100 input))))

(defn answer-part-2 [input]
  (let [input (apply concat (repeat 10000 input))
          out (FFT-optimized 100 input)
          offset (edn/read-string (apply str (take 7 input)))]
    (apply str (take 8 (drop offset out)))))

(deftest part1
  (is (= "67481260" (answer-part-1))))

(deftest example
  (is (= "24176176" (apply str (take 8
         (FFT-optimized 100 [8 0 8 7 1 2 2 4 5 8 5 9 1 4 5 4 6 6 1 9 0 8 3 2 1 8 6 4 5 5 9 5])))))
  (is (= "24176176" (apply str (take 8
         (FFT 100 [8 0 8 7 1 2 2 4 5 8 5 9 1 4 5 4 6 6 1 9 0 8 3 2 1 8 6 4 5 5 9 5]))))))
