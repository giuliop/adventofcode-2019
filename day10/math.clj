(ns math)

(defn abs "(abs n) is the absolute value of n" [n]
  (cond
    (not (number? n)) (throw (IllegalArgumentException.
                               "abs requires a number"))
    (neg? n) (- n)
    :else n))

(defn gcd "(gcd a b) returns the greatest common divisor of a and b" [a b]
  (if (or (not (integer? a)) (not (integer? b)))
    (throw (IllegalArgumentException. "gcd requires two integers"))
    (loop [a (abs a) b (abs b)]
      (if (zero? b) a,
        (recur b (mod a b))))))



