(ns cljbox2d.core
  "This is [cljbox2d](https://github.com/floybix/cljbox2d/).

   A clojure wrapper for [JBox2D](http://www.jbox2d.org/), which is a
   close Java port of Erin Catto's excellent C++
   [Box2D](http://www.box2d.org/) physics engine.

   In this namespace we have the core API for fixtures, bodies and the
   World."
  (:import (org.jbox2d.common Vec2)
           (org.jbox2d.dynamics Body BodyDef BodyType Fixture FixtureDef World)
           (org.jbox2d.collision AABB WorldManifold)
           (org.jbox2d.collision.shapes PolygonShape CircleShape ShapeType)
           (org.jbox2d.callbacks QueryCallback)
           (org.jbox2d.dynamics.joints Joint)))

;; ## Basic data

(defn vec2
  "Make a `Vec2` object from given (`x`, `y`).
  If the argument is already a Vec2, return it."
  ([v] (if (isa? (class v) Vec2)
         v
         (Vec2. (first v) (second v))))
  ([x y] (Vec2. x y)))

(defn xy
  "Makes a vector [x y] from a Vec2, or convert polar coordinates [mag
angle] (radians) to [x y]."
  ([^Vec2 v]
     [(.x v) (.y v)])
  ([mag angle]
     [(* mag (Math/cos angle))
      (* mag (Math/sin angle))]))

;; ## World

(def ^:dynamic ^World *world*
  "The current Box2D World: see `create-world`.")

(def world-time "Simulated time passed in seconds" (atom nil))

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
this directly, instead use `(body!)` or `(fixture!)`"
  ;; TODO: filter (contact filtering)
  [{:keys [shape density friction restitution is-sensor
           user-data]
    :or {density 1, friction 0.3, restitution 0.3,
         is-sensor false}}]
  (let [fd (FixtureDef.)]
    (set! (.shape fd) shape)
    (set! (.density fd) density)
    (set! (.friction fd) friction)
    (set! (.restitution fd) restitution)
    (set! (.isSensor fd) is-sensor)
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
directly, instead use `(body!)`"
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
     (letfn [(nextstep [fl]
               (when fl (cons fl (nextstep (.getNext fl)))))]
       (lazy-seq (nextstep (.getFixtureList body)))))
  ([]
     (mapcat fixtureseq (bodyseq))))

(defn fixture
  "Often a body will only have one fixture. This is a convenience
function to pull out the first fixture from a body."
  [^Body body]
  (first (fixtureseq body)))

;; ### Coordinates

(defn local-point
  "Return body-local coordinates for a given world point"
  [^Body body pt]
  (xy (.getLocalPoint body (vec2 pt))))

(defn world-point
  "Return world coordinates for a point in body-local coordinates,
   or for a body origin point"
  ([^Body body]
     (xy (.getPosition body)))
  ([^Body body pt]
     (xy (.getWorldPoint body (vec2 pt)))))

(defn local-center
  "Center of mass of a body in local coordinates"
  [^Body body]
  (xy (.getLocalCenter body)))

(defn world-center
  "Center of mass of a body in world coordinates"
  [^Body body]
  (xy (.getWorldCenter body)))

(defn local-coords
  "Local coordinates for a polygon (vertices) or circle (center)."
  [^Fixture fixt]
  (let [shp (.getShape fixt)]
    (case (shape-type fixt)
      :circle (map xy [(.getVertex ^CircleShape shp 0)])
      :polygon (let [n (.getVertexCount ^PolygonShape shp)]
                 (take n (map xy (.getVertices ^PolygonShape shp)))))))

(defn world-coords
  "World coordinates for a polygon (vertices) or circle (center)."
  [^Fixture fixt]
  (let [body (.getBody fixt)]
    (map (partial world-point body) (local-coords fixt))))

(defn radius
  "Radius of a Fixture's shape."
  [^Fixture fixt]
  (.m_radius (.getShape fixt)))

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

;; ## Contacts

(defn contact-data
  "Returns a map with keys :fixture-a :fixture-b :points :normal
   from a JBox2D Contact class. Returns nil if no contact points exist."
  [contact]
  (let [world-manifold (WorldManifold.) ;; TODO could pass this in
        manifold (.getManifold contact)
        pcount (.pointCount manifold)]
    (when (pos? pcount)
      ;; mutates its argument:
      (.getWorldManifold contact world-manifold)
      (let [fixt-a (.getFixtureA contact)
            fixt-b (.getFixtureB contact)
            -points (.points world-manifold)
            pts (map xy (take pcount -points))
            normal (xy (.normal world-manifold))]
        {:fixture-a fixt-a :fixture-b fixt-b
         :points pts :normal normal}))))

(defn contacting
  "Set of other bodies that the given body is currently contacting."
  [^Body body]
  (loop [cl (.getContactList body)
         result #{}]
    (if cl
      (recur (.next cl) (conj result (.other cl)))
      result)))

(defn contacts
  "Lazy seq of contacts on this body. Each contact is a map as defined
by the `contact-data` function. Contacts without contact points are exluded."
  [^Body body]
  (letfn [(nextstep [cl]
            (when cl
              (if-let [cdata (contact-data (.contact cl))]
                (cons cdata (nextstep (.next cl)))
                (nextstep (.next cl)))))]
    (lazy-seq (nextstep (.getContactList body)))))

;; ## Body properties

(defn angle
  "Angle of a body in radians"
  [^Body body]
  (.getAngle body))

(defn mass
  "Total mass of a body in kg"
  [^Body body]
  (.getMass body))

(defn linear-velocity
  "Linear velocity of the center of mass of a body. In m/s?"
  [^Body body]
  (xy (.getLinearVelocity body)))

(defn angular-velocity
  "Angular velocity of a body in radians/second."
  [^Body body]
  (xy (.getAngularVelocity body)))

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

(defn user-data
  [^Body body]
  (.getUserData body))

(defprotocol Destroyable
  "Abstraction for JBox2D objects which can be destroyed"
  (destroy! [this]))

(extend-protocol Destroyable
  Body
  (destroy! [this] (.destroyBody *world* this))
  Joint
  (destroy! [this] (.destroyJoint *world* this))
  Fixture
  (destroy! [this] (.destroyFixture (body this) this)))
    
;; ## Utilities

(def ^:const ^{:doc "Pi."} PI (. Math PI))

(defn angle-v
  "Angle of a vector in radians"
  [[x y]]
  (Math/atan2 y x))

(defn mag-v
  "Magnitude of a vector"
  [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn scale-v
  "Multiply elements of a vector by a scalar;
Default is to normalise to unit length."
  ([v]
     (scale-v v (/ 1 (mag-v v))))
  ([[x y] s]
     [(* x s) (* y s)]))

