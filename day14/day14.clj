(require '[clojure.string :as string]
         '[clojure.edn :as edn])

(defn key-and-quantity [s]
  "Takes a string like '29 RZCPW' and return {:RZCPW 29}"
  (let [[v k] (string/split s #" ")]
    {k (edn/read-string v)}))

(defn inputs-output [formula]
  "Takes a string like '29 RZCPW, 3 VXSLT => 9 CMRGJ' and returns
  {'CMRGJ' {:out 9 :in {'RZCPW' 29, 'VXSLT' 3}}}"
  (let [inputs-end (dec (string/index-of formula \=))
        output-start (+ 2 (string/index-of formula \>))
        inputs (->> (string/split (subs formula 0 inputs-end) #", ")
                    (map key-and-quantity)
                    (reduce into {}))
        [out-k out-v ] (first (key-and-quantity (subs formula output-start)))]
    {out-k {:out out-v :in inputs}}))

(def formulas
  "A dict of formulas with entries like '29 RZCPW, 3 VXSLT => 9 CMRGJ'"
  (->> (string/split-lines (string/trim (slurp "input_day14")))
       (map inputs-output)
       (reduce into {})))

(defn make [[ore to-make available] [elem quantity]]
  "Takes an elem and quantity to be made and the current state of
  [ore to-make available] and returns the updated state"
  (if (= "ORE" elem) [(+ ore quantity) to-make available]
    (let [wip-to-use ((fnil min 0) (available elem) quantity)
          to-make (update to-make elem (fnil + 0) (- quantity wip-to-use))
          available (update available elem (fnil - 0) wip-to-use)]
      [ore to-make available])))

(defn scale [formula factor]
  "Takes a formula and a scale factor and returns a formula with scaled
  output and input quantities"
  (let [out (* factor (:out formula))
        in (map (fn [[k v]] [k (* factor v)]) (:in formula))]
    {:out out :in (into {} in)}))

(defn ORE-for-FUEL
  ([fuel-quantity]
   "Returns the amount of ORE needed to make n FUEL given formulas"
   (ORE-for-FUEL fuel-quantity formulas {}))
  ([fuel-quantity formulas wip]
   (let [formulas (update formulas "FUEL" scale fuel-quantity)]
     (loop [ore 0
            to-make (:in (formulas "FUEL"))
            wip wip]
       (if (empty? to-make) [ore (:out (formulas "FUEL"))
                             (into {} (remove (comp zero? val) wip))]
         (let [[elem quantity] (first to-make)
               form (formulas elem)
               ;_ (when (> 3 (count to-make)) (println to-make))
               form-times (bigint (Math/ceil (/ quantity (:out form))))
               out (* form-times (:out form))
               needed (map (fn [[k v]] [k (* form-times v)]) (:in form))
               [ore to-make wip] (reduce make [ore (dissoc to-make elem)
                                               wip] needed)
               wip (update wip elem (fnil + 0) (- out quantity))]
           (recur ore to-make wip)))))))

(defn answer-part-1 []
  (first (ORE-for-FUEL 1)))

(def total-ore 1000000000000)
(def min-ore (answer-part-1)) ; ORE for 1 FUEL at no wip

(defn answer-part-2 []
  (loop [n (bigint (/ total-ore min-ore))
         old-fuel 1]
    (let [[ore fuel _] (ORE-for-FUEL n)
          ore-left (- total-ore ore)]
      (cond (zero? ore-left) fuel
            (neg? ore-left) old-fuel
            :else (let [delta (max 1 (bigint (/ ore-left min-ore)))]
                    (recur (+ n delta) fuel))))))
