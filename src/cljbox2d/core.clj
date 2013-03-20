(ns cljbox2d.core
  "[cljbox2d](https://github.com/floybix/cljbox2d/), a 2D physics engine.

   A clojure wrapper for [JBox2D](http://www.jbox2d.org/), which is a
   close Java port of Erin Catto's excellent C++
   [Box2D](http://www.box2d.org/) physics engine.

   In this namespace we have the core API for fixtures, bodies and the
   World."
  (:use [cljbox2d.vec2d :only [polar-xy v-add v-sub v-interp PI TWOPI
                               in-pi-pi edge-point-from-vertices v-mag]])
  (:import (org.jbox2d.common Vec2)
           (org.jbox2d.dynamics Body BodyDef BodyType Fixture FixtureDef World)
           (org.jbox2d.collision AABB WorldManifold)
           (org.jbox2d.collision.shapes PolygonShape CircleShape ShapeType MassData)
           (org.jbox2d.callbacks QueryCallback RayCastCallback)
           (org.jbox2d.dynamics.joints Joint)
           (org.jbox2d.dynamics.contacts Contact ContactEdge)))

(defn vec2
  "Make a JBox2D `Vec2` object from given `x`, `y`."
  ([v] (Vec2. (first v) (second v)))
  ([x y] (Vec2. x y)))

(defn v2xy
  "Makes a vector [x y] from a Vec2"
  [^Vec2 v]
  [(.x v) (.y v)])

;; ## World

(defonce ^{:doc "The current Box2D World: see `create-world!`."}
  ^:dynamic ^World *world* nil)

(defonce ^{:doc "Simulated time passed in seconds"}
  world-time (atom nil))

(defn create-world!
  "Create a new Box2D world. Gravity defaults to -10 m/s^2."
  ([]
     (create-world! [0 -10]))
  ([gravity]
     (reset! world-time 0.0)
     (alter-var-root (var *world*) (fn [_] (World. (vec2 gravity) true)))))

(defn step!
  "Simulate the world for a time step given in seconds.
   Note that Box2D objects are locked during simulation."
  ([dt]
     (step! dt 8 3))
  ([dt velocity-iterations position-iterations]
     (swap! world-time + dt)
     (.step *world* dt velocity-iterations position-iterations)))

;; ## Enums

(def ^{:private true}
  body-types
  {:dynamic BodyType/DYNAMIC
   :static BodyType/STATIC
   :kinematic BodyType/KINEMATIC})

(def ^{:private true}
  body-keywords
  (zipmap (vals body-types) (keys body-types)))

(defn body-type
  "The body type as a keyword `:dynamic` `:static` or `:kinematic`."
  [^Body body]
  (body-keywords (.getType body)))

(def ^{:private true}
  shape-types
  {:circle ShapeType/CIRCLE
   :polygon ShapeType/POLYGON})

(def ^{:private true}
  shape-keywords
  (zipmap (vals shape-types) (keys shape-types)))

(defn shape-type
  "The shape type of a Fixture as a keyword `:circle` or `:polygon`."
  [^Fixture fixt]
  (shape-keywords (.getType fixt)))

;; ## Creation of objects

;; ### Shapes

(defn circle
  "Create a circle shape, by default centered at [0 0]"
  ([radius]
     (circle radius [0 0]))
  ([radius center]
     (let [shape (CircleShape.)]
       (set! (. shape m_radius) radius)
       (.set (. shape m_p) (vec2 center))
       shape)))

(defn edge
  "Create an edge shape, a line between two points. Good for static
bodies but seems to behave strangely in dynamic ones."
  [pt1 pt2]
  (let [shape (PolygonShape.)]
    (.setAsEdge shape (vec2 pt1) (vec2 pt2))
    shape))

(defn box
  "Create a box shape from half-width, half-height,
by default centered at [0 0]"
  ([hx hy]
     (let [shape (PolygonShape.)]
       (.setAsBox shape hx hy)
       shape))
  ([hx hy center]
     (box hx hy center 0))
  ([hx hy center angle]
     (let [shape (PolygonShape.)]
       (.setAsBox shape hx hy (vec2 center) angle)
       shape)))

(defn rod
  "Create a long thin (box) shape extending from a point to a given
   length and with a given angle."
  [from-pt angle length width]
  (box (/ length 2) (/ width 2)
       (v-add from-pt (polar-xy (/ length 2) angle))
       angle))

(defn polygon
  "Create a polygon shape. Must be convex!
   It is assumed that the exterior is the right of each edge.
   i.e. vertices go counter-clockwise."
  [vertices]
  (let [shape (PolygonShape.)
        va (into-array Vec2 (map vec2 vertices))]
    (.set shape va (count vertices))
    shape))

;; ### Fixtures

(defn fixture-def
  "A FixtureDef: a shape with some physical properties. Do not call
this directly, instead use `(body!)` or `(fixture!)`.

`:group-index` allows a certain group of objects to never
collide (negative) or always collide (positive). Zero means no
collision group.

`:user-data` should be an atom holding a map."
  [{:keys [shape density friction restitution is-sensor
           group-index user-data]
    :or {density 1, friction 0.3, restitution 0.3,
         is-sensor false, group-index 0}}]
  (let [fd (FixtureDef.)
        ff (.filter fd)]
    (set! (.shape fd) shape)
    (set! (.density fd) density)
    (set! (.friction fd) friction)
    (set! (.restitution fd) restitution)
    (set! (.isSensor fd) is-sensor)
    (set! (.groupIndex ff) group-index)
    (set! (.userData fd) user-data)
    fd))

(defn fixture!
  "Creates a Fixture on an existing Body. The second argument is a
fixture specification map to be passed to the `fixture-def` function."
  [^Body body fixture-spec]
  (.createFixture body (fixture-def fixture-spec)))

;; ### Bodies

(defn body-def
  "A BodyDef, which holds properties but not shapes. Do not call this
directly, instead use `(body!)`.

`:user-data` should be an atom holding a map."
  [{:keys [type position angle bullet fixed-rotation
           angular-damping linear-damping
           angular-velocity linear-velocity
           user-data]
    :or {type :dynamic, position [0 0], angle 0,
         bullet false, fixed-rotation false,
         angular-damping 0, linear-damping 0,
         angular-velocity 0, linear-velocity [0 0]}}]
  (let [bd (BodyDef.)]
    (set! (.type bd) (body-types type))
    (set! (.position bd) (vec2 position))
    (set! (.angle bd) angle)
    (set! (.bullet bd) bullet)
    (set! (.fixedRotation bd) fixed-rotation)
    (set! (.angularDamping bd) angular-damping)
    (set! (.linearDamping bd) linear-damping)
    (set! (.angularVelocity bd) angular-velocity)
    (set! (.linearVelocity bd) (vec2 linear-velocity))
    (set! (.userData bd) user-data)
    bd))

(defn body!
  "Creates a Body together with some Fixtures.
  The first argument is a body specification map to be passed to the
`body-def` function. Any remaining arguments are fixture specification
maps to be passed to the `fixture-def` function."
  [body-spec & fixture-specs]
  (let [bd (body-def body-spec)
        bod (.createBody *world* bd)]
    (doseq [fspec fixture-specs]
      (fixture! bod fspec))
    bod))

;; ## Query of objects

(defn ^Body body
  "Get the body to which a fixture belongs"
  [^Fixture fixt]
  (.getBody fixt))

(defn bodyseq
  "Lazy seq of all bodies in the world, or a body list"
  ([]
     (bodyseq (.getBodyList *world*)))
  ([^Body body]
     (lazy-seq (when body (cons body (bodyseq (.getNext body)))))))

(defn fixtureseq
  "Lazy seq of fixtures on a body or (concatenated) all in the world"
  ([^Body body]
     (letfn [(nextstep [^Fixture fl]
               (when fl (cons fl (nextstep (.getNext fl)))))]
       (lazy-seq (nextstep (.getFixtureList body)))))
  ([]
     (mapcat fixtureseq (bodyseq))))

(defn fixture
  "Often a body will only have one fixture. This is a convenience
function to pull out the first fixture from a body."
  [^Body body]
  {:pre [(= 1 (count (fixtureseq body)))]}
  (first (fixtureseq body)))

;; ### Coordinates

(defprotocol TwoDeeObject
  "General methods for 2D physical objects which may be aggregated"
  (mass [this] "Total mass in kg.")
  (center [this] "Center of mass in world coordinates.")
  (loc-center [this] "Center of mass in local coordinates.")
  (position [this] [this loc-pt] "World coordinates of a local point, default [0 0].")
  (to-local [this pt] "Local coordinates of a world point.")
  (edge-point* [this angle frac origin-pt] "World coordinates a
   fraction of the way to the edge of a shape in a given direction
   from an origin point. Prefer the high-level `edge-point`.")
  (user-data [this] "Returns the user data item. By convention this should be an atom holding a map."))

(declare world-coords local-coords)

(extend-protocol TwoDeeObject

  Body
  (mass [this]
    (.getMass this))
  (center [this]
    (v2xy (.getWorldCenter this)))
  (loc-center [this]
    (v2xy (.getLocalCenter this)))
  (position
    ([this]
       (v2xy (.getPosition this)))
    ([this loc-pt]
       (v2xy (.getWorldPoint this (vec2 loc-pt)))))
  (to-local [this pt]
    (v2xy (.getLocalPoint this (vec2 pt))))
  (edge-point* [this angle frac origin-pt]
    ;; take the edge point from all fixtures having
    ;; greatest dot product with the (unit) angle vector.
    (let [fx-pts (for [fx (fixtureseq this)]
                   (edge-point* fx angle frac origin-pt))]
      (apply max-key #(v-mag (v-sub % origin-pt))
             (remove nil? fx-pts))))
  (user-data [this] (.getUserData this))

  Fixture
  (mass [this]
    (let [md (MassData.)]
      (.getMassData this md)
      (.mass md)))
  (center [this]
    (position this (loc-center this)))
  (loc-center [this]
    (let [md (MassData.)]
      (.getMassData this md)
      (v2xy (.center md))))
  (position
    ([this]
       (position (body this)))
    ([this loc-pt]
       (position (body this) loc-pt)))
  (to-local [this pt]
    (to-local (body this) pt))
  (edge-point* [this angle frac origin-pt]
    (let [vv (world-coords this)
          on-edge (edge-point-from-vertices vv angle origin-pt)]
      (if (nil? on-edge) nil
          (v-interp origin-pt on-edge frac))))
  (user-data [this] (.getUserData this)))

(defn edge-point
  "World coordinates on the edge of an object in a given direction
from its center. Further arguments can specify the fraction out
towards the edge (or outside if > 1), and an origin point other than
the object center (in world coordinates)."
  ([this angle] (edge-point* this angle 1 (center this)))
  ([this angle frac] (edge-point* this angle frac (center this)))
  ([this angle frac origin-pt] (edge-point* this angle frac origin-pt)))

(defn radius
  "Radius of a Fixture's shape."
  [^Fixture fixt]
  (.m_radius (.getShape fixt)))

(defn local-coords
  "Local coordinates of polygon vertices. Approximated for circles."
  [^Fixture fixt]
  (let [shp (.getShape fixt)]
    (case (shape-type fixt)
      :circle (let [r (radius fixt)
                    cent (loc-center fixt)]
                (for [a (range (- PI) PI (/ TWOPI 30))]
                  (v-add cent (polar-xy r a))))
      :polygon (let [n (.getVertexCount ^PolygonShape shp)]
                 (take n (map v2xy (.getVertices ^PolygonShape shp)))))))

(defn world-coords
  "World coordinates of polygon vertices. Approximated for circles."
  [^Fixture fixt]
  (let [body (.getBody fixt)]
    (map #(position body %) (local-coords fixt))))

(defn angle
  "Angle of a body in radians"
  [^Body body]
  (in-pi-pi (.getAngle body)))

;; ## Movement

(defn linear-velocity
  "Linear velocity of a point on the body in local coordinates, by
   default its center of mass. In m/s."
  ([^Body body]
     (v2xy (.getLinearVelocity body)))
  ([^Body body loc-pt]
     (v2xy (.getLinearVelocityFromLocalPoint body (vec2 loc-pt)))))

(defn angular-velocity
  "Angular velocity of a body in radians/second."
  [^Body body]
  (.getAngularVelocity body))

(defn apply-force!
  "Apply a force in Newtons to body at a world point. If the force
is not applied at the center of mass, it will generate a torque and
affect the angular velocity. This wakes up the body."
  [^Body body force pt]
  (.applyForce body (vec2 force) (vec2 pt)))

(defn apply-torque!
  "Apply a torque in N-m, i.e. about the z-axis (out of the
screen). This affects the angular velocity without affecting the
linear velocity of the center of mass. This wakes up the body."
  [^Body body torque]
  (.applyTorque body torque))

(defn apply-impulse!
  "Apply an impulse in N-seconds or kg-m/s at a point. This
immediately modifies the velocity. It also modifies the angular
velocity if the point of application is not at the center of
mass. This wakes up the body."
  [^Body body impulse pt]
  (.applyLinearImpulse body (vec2 impulse) (vec2 pt)))

(defn awake?
  [^Body body]
  (.isAwake body))

(defn wake!
  "Wake up a body."
  [^Body body]
  (.setAwake body true))

(defn sleep!
  "Put a body to sleep."
  [^Body body]
  (.setAwake body false))

(defprotocol Destroyable
  "Abstraction for JBox2D objects which can be destroyed"
  (destroy! [this] "Remove object from the World permanantly."))

(extend-protocol Destroyable
  Body
  (destroy! [this] (.destroyBody *world* this))
  Joint
  (destroy! [this] (.destroyJoint *world* this))
  Fixture
  (destroy! [this] (.destroyFixture (body this) this)))

;; ### Spatial queries

(defn aabb
  "Axis-Aligned Bounding Box"
  ([[x0 y0] [x1 y1]]
     (AABB. (vec2 [(min x0 x1) (min y0 y1)])
            (vec2 [(max x0 x1) (max y0 y1)])))
  ([^Fixture fixt]
     (.getAABB fixt)))

(defn query-aabb
  "Return a vector of (up to a given number of) fixtures overlapping
an Axis-Aligned Bounding Box"
  ([^AABB bb]
     (query-aabb bb 1000000))
  ([^AABB bb max-take]
     (let [fxx (atom [])
           cb (reify QueryCallback
                (reportFixture [_ fixt]
                  (swap! fxx conj fixt)
                  ;; return false to end search
                  (< (count @fxx) max-take)))]
       (.queryAABB *world* cb bb)
       @fxx)))

(defn query-at-point
  "Return a vector of fixtures overlapping the given point. The point
is tested to be inside each shape, not just within its bounding box."
  ([pt]
     (query-at-point pt 1000000))
  ([[x y] max-take]
     (let [bb (aabb [(- x 0.001) (- y 0.001)]
                    [(+ x 0.001) (+ y 0.001)])
           fxx (atom [])
           pt-vec2 (vec2 [x y])
           cb (reify QueryCallback
                (reportFixture [_ fixt]
                  (if (.testPoint fixt pt-vec2)
                    (swap! fxx conj fixt))
                  ;; return false to end search
                  (< (count @fxx) max-take)))]
       (.queryAABB *world* cb bb)
       @fxx)))

(defn raycast
  "Raycast from start-pt to end-pt, returning the intersected fixtures
in an ordered seq. Each element of the seq looks like `[fixture point
normal fraction]`, giving details of the intersection point and its
fraction along the ray."
  [start-pt end-pt]
  (let [fxx (atom [])
        cb (reify RayCastCallback
             (reportFixture [_ fixt pt norm frac]
               (swap! fxx conj [fixt pt norm frac])
               1.0))]
    (.raycast *world* cb start-pt end-pt)
    (sort-by #(nth % 3) @fxx)))

;; ## Contacts

(defn contact-data
  "Returns a map with keys :fixture-a :fixture-b :points :normal
   from a JBox2D Contact class. Returns nil if no contact points exist."
  [^Contact contact]
  (let [world-manifold (WorldManifold.) ;; TODO could pass this in
        manifold (.getManifold contact)
        pcount (.pointCount manifold)]
    (when (pos? pcount)
      ;; mutates its argument:
      (.getWorldManifold contact world-manifold)
      (let [fixt-a (.getFixtureA contact)
            fixt-b (.getFixtureB contact)
            -points (.points world-manifold)
            pts (map v2xy (take pcount -points))
            normal (v2xy (.normal world-manifold))]
        {:fixture-a fixt-a :fixture-b fixt-b
         :points pts :normal normal}))))

(defn contacts
  "Lazy seq of contacts on this body. Each contact is a map as defined
by the `contact-data` function. Contacts without actual contact
points (i.e. created only due to overlapping bounding boxes) are
excluded."
  [^Body bod]
  (letfn [(nextstep [^ContactEdge cl]
            (when cl
              (if-let [cdata (contact-data (.contact cl))]
                (cons cdata (nextstep (.next cl)))
                (nextstep (.next cl)))))]
    (lazy-seq (nextstep (.getContactList bod)))))

(defn contacting
  "Set of other bodies that the given body is currently contacting."
  [^Body bod]
  (let [bodies (mapcat #(list (body (:fixture-a %))
                              (body (:fixture-b %)))
                       (contacts bod))]
    (set (remove #(= bod %) bodies))))
