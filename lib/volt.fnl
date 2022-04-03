(fn v->f [v zero-freq]
  "Convert a voltage to a frequency given a base frequency that represents zero
volts"
  (->> v
       (^ 2)
       (* zero-freq)))

(fn f->v [f zero-freq]
  "Convert a frequency to a voltage given a base frequency that represents zero
volts"
  (-> f
      (/ zero-freq)
      (math.log 2)))

(fn divide-v [v divisor]
  "Divide a voltage by the given divisor in the frequency domain"
  (- v (math.log divisor 2)))

(local max 5)
(local min (- max))

(fn clamp-v [v]
  "Clamp the given voltage to positive or negative 5 volts"
  (util.clamp v min max))

{: v->f : f->v : divide-v : clamp-v : max : min}

