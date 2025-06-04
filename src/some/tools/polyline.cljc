(ns some.tools.polyline
  (:require [some.tools.vec :as vec]))

(defn length
  "Returns the total length of a polyline."
  [polyline]
  (loop [l 0
         p polyline]
    (if (< (count p) 2) l
        (recur (+ l (vec/distance (first p) (second p))) (rest p)))))

(defn index-at-length
  "Given `length`, returns the interpolated index on `polyline` that
  corresponds to a point. If `length` is less than 0 or greater than the total
  length of `polyline`, returns `nil`."
  [polyline length]
  (if (< length 0.0) nil
      (loop [i 0
             j 1
             length length]
        (if (= j (count polyline)) nil
            (let [cur-length (vec/distance (nth polyline i) (nth polyline j))]
              (if (<= length cur-length)
                (+ i (/ length cur-length))
                (recur (inc i) (inc j) (- length cur-length))))))))

(defn point-at-index
  "Given an interpolated `index`, returns a point on `polyline`. Returns `nil`
  if `index` is less than 0.0 or out of bounds."
  [polyline index]
  (cond
    (or (< index 0.0) (> index (dec (count polyline)))) nil
    (= index 0.0) (first polyline)
    :else (let [i (Math/ceil (dec index))
                p (- index i)]
            (when (< i (dec (count polyline)))
              (vec/mix (nth polyline i) (nth polyline (inc i)) p)))))

(defn resample-by-length
  "Returns a new polyline where the distance between every point is `length`,
  based on `polyline`."
  [polyline length & {:keys [closed] :or {closed false}}]
  (if (true? closed)
    (conj (resample-by-length polyline length) (first polyline))
    (loop [distance length
           vertices [(first polyline)]]
      (let [index (index-at-length polyline distance)]
        (if (nil? index) vertices
            (recur (+ distance length)
                   (conj vertices (point-at-index polyline index))))))))

(defn resample-by-count
  "Returns a new polyline with `n` evenly-spaced vertices."
  [polyline n & {:keys [closed] :or {closed false}}]
  (let [num-pts (if (true? closed) (dec n) n)]
    (resample-by-length polyline (/ (length polyline) num-pts) :closed closed)))

(defn render
  "Framework agnostic function to render the polyline. Requires a rendering function.

  The rendering function will be called like so: `(render-fn v1 v2)`, where `v1`
  and `v2` are vectors.

  Arguments:
  - `render-fn` - rendering function
  - `polyline` - vector containing 2D or 3D vectors representing polyline
  vertices

  Keys:
  - `:start` - index of polyline vertex to start drawing from. (Default: 0)
  - `:n` - number of vertices to draw. (Default: length of polyline)"
  [render-fn polyline & {:keys [start n] :or {start 0}}]
  (let [n (if (nil? n) (count polyline) n)]
    (doseq [i (range (dec n))]
      (render-fn (nth polyline (+ start i))
                 (nth polyline (inc (+ start i)))))))
