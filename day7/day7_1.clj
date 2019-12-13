(require '[intcode-computer :as computer]
           '[clojure.set :as sets])

(def input-signal 0)

(defn input-program []
  "Return the input memory"
  (vec (map #(. Integer parseInt %)
            (str/split (str/trim (slurp "input_day7")) #","))))

(defn output-signal [input program]
  "Run input and program on the intcode-computer and return the last output
  value"
  (last (first (computer/run input program))))

(defn try-settings [input-signal settings program]
  "Takes a sequence of settings for the amplifiers, an input signal, and
  a prgram and retun the output signal"
  (reduce (fn [item acc] (output-signal [acc item] program))
          input-signal
          settings))

(defn answer []
  "Return the quiz answer"
  (apply max
         (for [x1 (range 5)x2 (range 5) x3 (range 5)
               x4 (range 5) x5 (range 5)
               :when (= 5 (count (set [x1 x2 x3 x4 x5])))]
           (try-settings input-signal [x1 x2 x3 x4 x5] (input-program)))))

