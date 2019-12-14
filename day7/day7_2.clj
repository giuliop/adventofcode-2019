(require '[intcode-computer :as computer]
         '[clojure.set :as sets])

(def input-signal 0)

(defn input-program []
  "Return the input memory"
  (vec (map #(. Integer parseInt %)
            (str/split (str/trim (slurp "input_day7")) #","))))

(defn output-signal [input program pc]
  "Run input and program at program-counter pc on the intcode-computer
  and return [[outputs] [memory] [inputs] program-counter ended?]"
  (computer/run input program pc false))

(defn try-settings [input-signal settings program]
  "Takes a sequence of settings for the amplifiers, an input signal, and
  a program and retun the output signal"
  (let [[s1 s2 s3 s4 s5] settings]
    (loop [memories (vec (repeat 5 program)) ; memory of the 5 amplifiers
           inputs [[s1 input-signal] [s2] [s3] [s4] [s5]]
           pcs [0 0 0 0 0] ; program counters of the 5 amplifiers
           amp 0] ; current ampliifer running its program (0-4)
      (let [[m i pc] (map #(% amp) [memories inputs pcs])
            [o m i pc ended?] (output-signal i m pc)]
        (if ended? (last i)
          (let [next-amp (mod (inc amp) 5)
                next-amp-input (conj (inputs next-amp) (last o))]
            (recur (assoc memories amp m)
                   (assoc (assoc inputs next-amp next-amp-input)
                          amp i)
                   (assoc pcs amp pc)
                   next-amp)))))))

(defn answer []
  "Return the quiz answer"
  (apply max
         (for [x1 (range 5 10) x2 (range 5 10) x3 (range 5 10)
               x4 (range 5 10) x5 (range 5 10)
               :when (= 5 (count (set [x1 x2 x3 x4 x5])))]
           (try-settings input-signal [x1 x2 x3 x4 x5] (input-program)))))

