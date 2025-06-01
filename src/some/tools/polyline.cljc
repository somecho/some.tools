(ns some.tools.polyline)

(defn gen-circle2
  "Generates a 2D polyline representing the vertices of a circle.
  Arguments:
  - `cx` - x-coordinate of circle center
  - `cy` - y-coordinate of circle center
  - `radius` - circle's radius

  Keys:
  - `:res` - number of segments to represent the circle with. (Default: 72)"
  [cx cy radius & {:keys [res] :or {res 72}}]
  (loop [i 0
         vertices []]
    (if (= i (inc res)) vertices
        (let [p (* Math/PI 2.0 (/ i res))
              x (+ cx (* radius (Math/cos p)))
              y (+ cy (* radius (Math/sin p)))]
          (recur (inc i) (conj vertices [x y]))))))

(defn gen-star2
  "Generates a star as a 2D polyline.
  Arguments:
  - `cx` - x-coordinate of star center
  - `cy` - y-coordinate of star center
  - `inner-radius` - radius of the inner points of the star
  - `outer-radius` - radius of the outer points of the star

  Keys:
  - `:num-points` - the number of points the star has. (Default: 5)"
  [cx cy inner-radius outer-radius & {:keys [num-points] :or {num-points 5}}]
  (let [num-vertices (* 2 num-points)]
    (loop [i 0
           vertices []]
      (if (= i (inc num-vertices)) vertices
          (let [deg (* Math/PI 2.0 (/ i num-vertices))
                r (if (even? i) outer-radius inner-radius)
                x (-> (Math/cos deg) (* r) (+ cx))
                y (-> (Math/sin deg) (* r) (+ cy))]
            (recur (inc i) (conj vertices [x y])))))))

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
