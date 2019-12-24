(require '[math])

(def moons [{:pos [-3  15 -11] :vel [0 0 0]}
            {:pos [3   13 -19] :vel [0 0 0]}
            {:pos [-13 18 -2 ] :vel [0 0 0]}
            {:pos [6   0  -1 ] :vel [0 0 0]}])

(defn gravity [moons]
  (let [grav-f (fn [x1 x2] (cond (< x1 x2) 1, (> x1 x2) -1, :else 0))]
    (reduce (fn [moons [num1 num2]]
              (let [m1 (moons num1)
                    m2 (moons num2)
                    v1-factors (map grav-f (:pos m1) (:pos m2))
                    v2-factors (map grav-f (:pos m2) (:pos m1))
                    m1 (assoc-in m1 [:vel] (map + (:vel m1) v1-factors))
                    m2 (assoc-in m2 [:vel] (map + (:vel m2) v2-factors))]
                (assoc (assoc moons num1 m1) num2 m2)))
            moons
            (for [x (range (count moons)), y (range (count moons))
                  :when (> y x)]
              [x y]))))

(defn velocity [moons]
  (let [new-pos (fn [pos vel] (map + pos vel))]
    (vec (map #(update-in % [:pos] new-pos (:vel %)) moons))))

(defn potential-energy [moon]
  (apply + (map math/abs (:pos moon))))

(defn kinetic-energy [moon]
  (apply + (map math/abs (:vel moon))))

(defn total-energy [moons]
  (apply + (map * (map potential-energy moons)
                  (map kinetic-energy moons))))

(defn step
  ([moons] (velocity (gravity moons)))
  ([moons n-steps] (nth (iterate step moons) n-steps)))

(defn answer-part-1 []
  (total-energy (n-steps moons 1000)))

(defn period [moons axis]
  (loop [moons (vec (map #(into {} {:pos [((:pos %) axis)]
                                    :vel [((:vel %)axis)]}) moons))
         acc #{}]
      (if (contains? acc moons) (count acc)
        (recur (step moons) (conj acc moons)))))

(defn answer-part-2 []
  (let [x-period (period moons 0)
        y-period (period moons 1)
        z-period (period moons 2)]
    (math/lcm (math/lcm x-period y-period) z-period)))
