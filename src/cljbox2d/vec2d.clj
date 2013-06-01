(ns cljbox2d.vec2d
  "2D geometric point and vector helpers.
   All in terms of clojure vectors [x y].")

(def ^:const ^{:doc "Pi (180 degrees)."} PI (. Math PI))
(def ^:const ^{:doc "2 Pi (360 degrees)."} TWOPI (* PI 2.0))
(def ^:const ^{:doc "Pi/2 (90 degrees)."} PI_2 (* PI 0.5))

(def x-val first)
(def y-val second)

(defn abs
  "Absolute value; avoids reflection from overloaded Math/abs"
  [x]
  (if (neg? x) (- x) x))

(defn in-pi-pi
  "Returns the angle expressed in the range -pi to pi."
  [angle]
  (cond
   (> angle PI) (in-pi-pi (- angle TWOPI))
   (< angle (- PI)) (in-pi-pi (+ angle TWOPI))
   :else angle))

(def ^{:private true} dir-angle
  (let [m {:right           0
           :top-right       (/ PI 4)
           :bottom-right (- (/ PI 4))
           :top             PI_2
           :bottom       (- PI_2)
           :top-left        (* PI (/ 3 4))
           :bottom-left  (- (* PI (/ 3 4)))
           :left            PI}]
    (merge m {:r  (:right m)
              :tr (:top-right m)
              :br (:bottom-right m)
              :t  (:top m)
              :b  (:bottom m)
              :tl (:top-left m)
              :bl (:bottom-left m)
              :l  (:left m)})))

(defn angle*
  "Return an angle in radians from given number in degrees, or a
  keyword for a direction:
  `:right (0) :top (pi/2) :left (pi) :bottom (-pi/2)`
  `:top-right :top-left :bottom-left :bottom-right`
  Aliases `:r :t :l :b :tr :tl :bl :br`."
  [a]
  (cond
   (number? a) (in-pi-pi (* PI (/ a 180.0)))
   (keyword? a) (a dir-angle)))

(defn angle-left?
  [angle]
  (> (abs (in-pi-pi angle)) PI_2))

(defn angle-up?
  [angle]
  (pos? (in-pi-pi angle)))

(defn vertical-angle?
  "Is angle vertical, within +/- 1.5 degrees by default."
  ([ang]
     (vertical-angle? ang (* 1.5 (/ PI 180.0))))
  ([ang tol]
     (< (abs (- (abs (in-pi-pi ang)) PI_2)) tol)))

(defn horizontal-angle?
  "Is angle horizontal, within +/- 1.5 degrees by default."
  ([ang]
     (vertical-angle? (in-pi-pi (+ ang PI_2))))
  ([ang tol]
     (vertical-angle? (in-pi-pi (+ ang PI_2)) tol)))

(defn polar-xy
  "Convert polar coordinates (magnitude, angle) to cartesian
   coordinates (x, y)."
  [mag angle]
  [(* mag (Math/cos angle))
   (* mag (Math/sin angle))])

(defn v-angle
  "Angle of a 2d geometric vector in radians in range -pi to pi."
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
  "Find a point `frac` fraction of the way from v1 to v2 by linear
   interpolation."
  [v1 v2 frac]
  (v-add (v-scale v2 frac)
         (v-scale v1 (- 1 frac))))

(defn v-dist
  "Distance from one 2d point to another."
  [v1 v2]
  (v-mag (v-sub v1 v2)))

(defn v-dot
  "Dot product of two vectors."
  [v1 v2]
  (apply + (map * v1 v2)))

(defn poly-flip-x
  "Flip polygon coordinates horizontally, by default around x=0."
  ([vv]
     (poly-flip-x vv 0))
  ([vv x0]
     ;; need to reverse to keep it counter-clockwise.
     (reverse (map (fn [[x y]] [(- x0 x) y]) vv))))

(defn poly-flip-y
  "Flip polygon coordinates vertically, by default around y=0."
  ([vv]
     (poly-flip-y vv 0))
  ([vv y0]
     ;; need to reverse to keep it counter-clockwise.
     (reverse (map (fn [[x y]] [x (- y0 y)]) vv))))

(defn edge-point-from-vertices
  "Finds point on edge of a convex polygon shape (given by
   counter-clockwise vertices) at a given `angle` from `origin-pt`."
  [vv angle origin-pt]
  (let [targ (in-pi-pi angle)
        ;; offsets from origin point (typically shape center)
        offs (map v-sub vv (repeat origin-pt))
        angs (map v-angle offs)
        ;; differences of angles from target
        errs (map #(in-pi-pi (- % targ)) angs)
        ;; index of vertex closest to target angle
        best-i (apply min-key #(abs (nth errs %))
                      (range (count vv)))
        best-err (nth errs best-i)
        ;; now find the vertex on opposite side of target.
        ;; we know that coordinates go counter-clockwise
        ;; (by box2d polygon definition)
        opp-i (-> (if (pos? best-err) (dec best-i) (inc best-i))
                  (mod (count vv)))
        opp-err (nth errs opp-i)]
    ;; note however: it is possible that the angle vec does not touch
    ;; shape at all. in that case we return nil.
    (if (= (compare 0 best-err)
           (compare 0 opp-err))
      nil
      (v-interp
       (nth vv best-i)
       (nth vv opp-i)
       (abs (/ best-err
               (+ (abs best-err) (abs opp-err))))))))

(defn interior-angle
  "Interior angle of a triple of points using law of cosines:
     cosC = (a^2 + b^2 - c^2) / (2ab)
   where, in a triangle, angle C is opposite side c."
  [[v0 v1 v2]]
  (let [a (v-dist v0 v1)
        b (v-dist v2 v1)
        c (v-dist v0 v2)]
    (Math/acos (/ (+ (* a a)
                     (* b b)
                     (- (* c c)))
                  (* 2 a b)))))

(defn line-intersection
  "Point of intersection of two lines in the plane.
   First line passes through [x0 y0] with gradient m0.
   Other line passes through [x1 y1] with gradient m1.
   Gradient can be passed as nil for vertical lines."
  [[x0 y0] m0 [x1 y1] m1]
  (cond
   (= m0 m1) nil
   ;; vertical line at x = x0
   (nil? m0)
   (let [y (+ (* m1 (- x0 x1)) y1)]
     [x0 y])
   ;; vertical line at x = x1
   (nil? m1)
   (let [y (+ (* m0 (- x1 x0)) y0)]
     [x1 y])
   ;; general equation
   ;; x = m0.x0 - m1.x1 + y1 - y0
   ;;     (m0 - m1)
   ;; y = m0 (x - x0) + y0
   :else
   (let [x (/ (+ (* m0 x0) (* -1 m1 x1) y1 (- y0))
              (- m0 m1))
         y (+ (* m0 (- x x0)) y0)]
     [x y])))

(defn angle-to-gradient
  [ang]
  (cond (vertical-angle? ang) nil
        (horizontal-angle? ang) 0.0
        :else (Math/tan ang)))

(defn angle-intersection
  "Point of intersection of two lines in the plane."
  [xy0 ang0 xy1 ang1]
  (let [m0 (angle-to-gradient ang0)
        m1 (angle-to-gradient ang1)]
    (line-intersection xy0 m0 xy1 m1)))
