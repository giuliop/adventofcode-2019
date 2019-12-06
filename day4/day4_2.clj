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
                   (or (and (= d1 d2) (not= d2 d3))
                       (and (= d2 d3) (not= d1 d2) (not= d3 d4))
                       (and (= d3 d4) (not= d2 d3) (not= d4 d5))
                       (and (= d4 d5) (not= d3 d4) (not= d5 d6))
                       (and (= d5 d6) (not= d4 d5))))]
    sol))

(defn answer []
  (count (solutions)))
