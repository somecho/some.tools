(ns some.tools.vec
  "This toolkit uses vectors like Clojure vectors and also mathematical
  constructs of any dimension e.g. 2D or 3D coordinates."
  (:require [some.tools.scalar :as scalar]))

(defn +
  "Returns the component-wise sum of `v1` and `v2`."
  [v1 v2] (mapv clojure.core/+ v1 v2))

(defn -
  "Returns the result of subtracting `v2` from `v1` component-wise."
  [v1 v2] (mapv clojure.core/- v1 v2))

(defn *
  "Returns the hadamard (component-wise) product of `v1` and `v2`"
  [v1 v2] (mapv clojure.core/* v1 v2))

(defn /
  "Returns the result of dividing `v1` by `v2` component-wise."
  [v1 v2] (mapv clojure.core// v1 v2))

(defn length
  "Returns the length of a vector of any dimension."
  [v] (->> (some.tools.vec/* v v) (reduce clojure.core/+) (Math/sqrt)))

(defn distance
  "Returns the length between two vectors."
  [v1 v2] (length (some.tools.vec/- v2 v1)))

(defn mix
  "Returns the linear interpolation between vectors `v1` and `v2` of the same
  dimension. `t` is a normalized value."
  [v1 v2 t] (mapv (fn [a b] (scalar/mix a b t)) v1 v2))

(defn dx->x
  "From a vector of numbers representing intervals, return a vector containing
  the discrete integration.

  For example:
  [1 1 1] -> [0 1 2 3]
  [1 2 3] -> [0 1 3 6]"
  [dx & {:keys [start] :or {start 0.0}}]
  (reduce (fn [res next]
            (conj res (clojure.core/+ (last res) next))) [start] dx))
