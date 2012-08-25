(ns cljbox2d.vec2d
  "2D geometric point and vector helpers.
   All in terms of clojure vectors [x y].")

(def ^:const ^{:doc "Pi (180 degrees)."} PI (. Math PI))
(def ^:const ^{:doc "2 Pi (360 degrees)."} TWOPI (* PI 2.0))
(def ^:const ^{:doc "Pi/2 (90 degrees)."} PI_2 (* PI 0.5))

(def dir-angle
  "Angles in radians for compass directions,
   with keys :n :ne :e :se :s :sw :w :nw."
  {:e 0
   :ne (/ PI 4)
   :n PI_2
   :nw (* PI (/ 3 4))
   :w PI
   :sw (* PI (/ 5 4))
   :s (* PI (/ 3 2))
   :se (* PI (/ 7 4))})

(defn polar-xy
  "Convert polar coordinates (magnitude, angle) to cartesian
coordinates [x y]."
  [mag angle]
  [(* mag (Math/cos angle))
   (* mag (Math/sin angle))])

(defn v-angle
  "Angle of a 2d geometric vector in radians"
  [[x y]]
  (Math/atan2 y x))

(defn v-mag
  "Magnitude of a 2d geometric vector"
  [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn v-scale
  "Multiply elements of a 2d vector by a scalar;
Default is to normalise to unit length."
  ([v]
     (v-scale v (/ 1 (v-mag v))))
  ([[x y] s]
     [(* x s) (* y s)]))

(defn v-sub
  "Subtract a 2d geometric vector from another (v1 - v2)."
  [v1 v2]
  (mapv - v1 v2))

(defn v-add
  "Add a 2d geometric vector to another."
  [v1 v2]
  (mapv + v1 v2))

(defn v-interp
  "Find a point `frac` fraction of the way from `v1` to `v2` by linear
interpolation."
  [v1 v2 frac]
  (v-add (v-scale v2 frac)
         (v-scale v1 (- 1 frac))))

(defn v-dist
  "Distance from one 2d point to another."
  [v1 v2]
  (v-mag (v-sub v1 v2)))
