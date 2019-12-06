(def solution-range [134792 675810])

(defn solutions []
  (for [d1 (range 1 7)
        d2 (range d1 10)
        d3 (range d2 10)
        d4 (range d3 10)
        d5 (range d4 10)
        d6 (range d5 10)
        :let [sol (+ (* d1 100000) (* d2 10000) (* d3 1000)
                           (* d4 100) (* d5 10) d6)]
        :when (and (< sol (second solution-range))
                   (> sol (first solution-range))
                   (or (= d1 d2) (= d2 d3) (= d3 d4) (= d4 d5) (= d5 d6)))]
    sol))

(defn answer []
  (count (solutions)))
