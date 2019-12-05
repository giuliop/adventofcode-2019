(defn masses []
  "Return the input module masses as a list of numbers"
  (->> (slurp "input_day1")
      (clojure.string/split-lines)
      (map #(. Integer parseInt %))))

(defn fuel-required [mass]
  "Takes a module mass and returns the fuel required to launch it"
  (-> mass
      (/ 3)
      (int)
      (- 2)))

(defn total-fuel-required [mass]
  "Include the fuel required for the fuel needed to launch a mass"
  (loop [fuel (fuel-required mass)
         extra-fuel (fuel-required fuel)]
    (if (not (pos? extra-fuel))
      fuel
      (recur (+ fuel extra-fuel) (fuel-required extra-fuel)))))

(defn total-fuel [masses]
  "Takes a list of masses and returns total fuel required to launch them"
  (reduce + (map total-fuel-required masses)))

(defn answer []
  "Return the quiz answer"
  (total-fuel (masses)))
