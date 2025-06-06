(ns some.tools.scalar)

(defn mix
  "Returns the linear interpolation between to scalar values `a` and `b`. `t` is
  a normalized value."
  [a b t] (+ (* (- 1.0 t) a) (* t b)))
