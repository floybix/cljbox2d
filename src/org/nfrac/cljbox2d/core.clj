(ns org.nfrac.cljbox2d.core
  "[cljbox2d](https://github.com/floybix/cljbox2d/), a 2D physics engine.

   A clojure wrapper for [JBox2D](http://www.jbox2d.org/), which is a
   close Java port of Erin Catto's excellent C++
   [Box2D](http://www.box2d.org/) physics engine."
  (:require [org.nfrac.cljbox2d.vec2d
             :refer [polar-xy v-add v-sub v-dist in-pi-pi PI TWOPI]])
  (:import (org.jbox2d.common Vec2)
           (org.jbox2d.dynamics World Body BodyDef BodyType Fixture FixtureDef)
           (org.jbox2d.collision AABB WorldManifold)
           (org.jbox2d.collision.shapes PolygonShape CircleShape EdgeShape
                                        ChainShape ShapeType MassData)
           (org.jbox2d.callbacks QueryCallback RayCastCallback ContactListener)
           (org.jbox2d.dynamics.contacts Contact ContactEdge)
           (org.jbox2d.dynamics.joints Joint JointType
                                       ConstantVolumeJoint ConstantVolumeJointDef
                                       DistanceJoint DistanceJointDef
                                       RopeJoint RopeJointDef
                                       MouseJoint MouseJointDef
                                       PrismaticJoint PrismaticJointDef
                                       RevoluteJoint RevoluteJointDef
                                       WeldJoint WeldJointDef)))

(defn vec2
  "Make a JBox2D `Vec2` object from given `x`, `y`."
  ([v] (Vec2. (first v) (second v)))
  ([x y] (Vec2. x y)))

(defn v2xy
  "Makes a vector [x y] from a Vec2"
  [^Vec2 v]
  [(.x v) (.y v)])

;; ## World

(defn new-world
  "Returns a new Box2D world. Gravity defaults to [0 -10] m/s^2."
  (^org.jbox2d.dynamics.World []
     (new-world [0 -10]))
  ([gravity]
     (World. (vec2 gravity))))

(defn step!
  "Simulate the world for a time step given in seconds.
   Note that Box2D objects are locked during simulation.
   Returns the world (although it remains a mutable object)."
  ([world dt]
     (step! world dt 8 3))
  ([^World world dt velocity-iterations position-iterations]
     (.step world dt velocity-iterations position-iterations)
     world))

(defn gravity!
  "Sets the gravity vector."
  [^World world gravity]
  (.setGravity world (vec2 gravity)))

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
   :polygon ShapeType/POLYGON
   :edge ShapeType/EDGE
   :chain ShapeType/CHAIN})

(def ^{:private true}
  shape-keywords
  (zipmap (vals shape-types) (keys shape-types)))

(defn shape-type
  "The shape type of a Fixture as a keyword e.g. `:circle` or `:polygon`."
  [^Fixture fixt]
  (shape-keywords (.getType fixt)))

(def ^{:private true}
  joint-types
  {:constant-volume JointType/CONSTANT_VOLUME
   :distance JointType/DISTANCE
   :rope JointType/ROPE
   :mouse JointType/MOUSE
   :prismatic JointType/PRISMATIC
   :revolute JointType/REVOLUTE
   :weld JointType/WELD})

(def ^{:private true}
  joint-keywords
  (zipmap (vals joint-types) (keys joint-types)))

(defn joint-type
  "The joint type as a keyword."
  [^Joint joint]
  (joint-keywords (.getType joint)))

;; ## Creation of objects

;; ### Shapes

(defn circle
  "Create a circle shape, by default centered at [0 0]"
  ([radius]
     (circle radius [0 0]))
  ([radius center]
     (let [shape (CircleShape.)]
       (.setRadius shape radius)
       (.set (.m_p shape) (vec2 center))
       shape)))

(defn edge
  "Create an edge shape, a line between two points."
  [pt1 pt2]
  (let [shape (EdgeShape.)]
    (.set shape (vec2 pt1) (vec2 pt2))
    shape))

(defn edge-chain
  "Creates a chain of edge shapes, assumed not self-intersecting, left
   open at the ends."
  [vertices]
  (let [shape (ChainShape.)
        va (into-array Vec2 (map vec2 vertices))]
    (.createChain shape va (count vertices))
    shape))

(defn edge-loop
  "Creates a loop of edge shapes, assumed not self-intersecting, where
   the last point joins back to the first point."
  [vertices]
  (let [shape (ChainShape.)
        va (into-array Vec2 (map vec2 vertices))]
    (.createLoop shape va (count vertices))
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

;; ### Userdata (like metadata)

(defprotocol UserData
  (user-data [this])
  (set-user-data! [this x]))

(extend-protocol UserData
  Body
  (user-data [this] (.getUserData this))
  (set-user-data! [this x] (.setUserData this x))
  Fixture
  (user-data [this] (.getUserData this))
  (set-user-data! [this x] (.setUserData this x))
  Joint
  (user-data [this] (.getUserData this))
  (set-user-data! [this x] (.setUserData this x)))

(defn vary-user-data
  "Alters the userdata attached to the object by applying `f` to any
   existing value or the empty map. Returns `this`."
  [this f]
  (set-user-data! this (f (or (user-data this) {})))
  this)

;; ### Fixtures

(defn fixture-def
  "A FixtureDef: a shape with some physical properties. Do not call
   this directly, instead use `(body!)` or `(fixture!)`.

   `:group-index` allows a certain group of objects to never
   collide (negative) or always collide (positive). Zero means no
   collision group."
  [{:keys [shape density friction restitution is-sensor
           group-index category-bits mask-bits
           user-data]
    :or {density 1, friction 0.3, restitution 0.3, is-sensor false
         group-index 0, category-bits 0x0001, mask-bits 0xFFFF}}]
  (let [fd (FixtureDef.)]
    (set! (.shape fd) shape)
    (set! (.density fd) density)
    (set! (.friction fd) friction)
    (set! (.restitution fd) restitution)
    (set! (.isSensor fd) is-sensor)
    (set! (.userData fd) user-data)
    (let [ff (.filter fd)]
      (set! (.groupIndex ff) group-index)
      (set! (.categoryBits ff) category-bits)
      (set! (.maskBits ff) mask-bits))
    fd))

(defn fixture!
  "Creates a Fixture on an existing Body. The second argument is a
   fixture specification map to be passed to the `fixture-def`
   function."
  [^Body body fixture-spec]
  (.createFixture body (fixture-def fixture-spec)))

;; ### Bodies

(defn body-def
  "A BodyDef, which holds properties but not shapes. Do not call this
   directly, instead use `(body!)`."
  [{:keys [type position angle bullet fixed-rotation gravity-scale
           angular-damping linear-damping
           angular-velocity linear-velocity
           user-data]
    :or {type :dynamic, position [0 0], angle 0,
         bullet false, fixed-rotation false, gravity-scale 1.0,
         angular-damping 0, linear-damping 0,
         angular-velocity 0, linear-velocity [0 0]}}]
  (let [bd (BodyDef.)]
    (set! (.type bd) (body-types type))
    (set! (.position bd) (vec2 position))
    (set! (.angle bd) angle)
    (set! (.bullet bd) bullet)
    (set! (.fixedRotation bd) fixed-rotation)
    (set! (.gravityScale bd) gravity-scale)
    (set! (.angularDamping bd) angular-damping)
    (set! (.linearDamping bd) linear-damping)
    (set! (.angularVelocity bd) angular-velocity)
    (set! (.linearVelocity bd) (vec2 linear-velocity))
    (set! (.userData bd) user-data)
    bd))

(defn body!
  "Creates a Body together with its Fixtures. The `body-spec` map is
   passed to the `body-def` function. Each of the `fixture-specs` are
   passed to the `fixture-def` function."
  [^World world body-spec & fixture-specs]
  (let [bd (body-def body-spec)
        bod (.createBody world bd)]
    (doseq [fspec fixture-specs]
      (fixture! bod fspec))
    bod))

;; ## Query of objects

(defn ^Body body-of
  "Get the body to which a fixture belongs"
  [^Fixture fixt]
  (.getBody fixt))

(defn bodyseq
  "Lazy seq of all bodies in the world, or a body list"
  ([^World world]
     (bodyseq world (.getBodyList world)))
  ([world ^Body body]
     (lazy-seq (when body (cons body (bodyseq world (.getNext body)))))))

(defn fixtureseq
  "Lazy seq of fixtures on a body."
  [^Body body]
  (letfn [(nextstep [^Fixture fl]
            (when fl (cons fl (nextstep (.getNext fl)))))]
    (lazy-seq (nextstep (.getFixtureList body)))))

(defn fixture-of
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
  (to-local-vect [this vect] "Local vector of a world vector."))

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
  (to-local-vect [this vect]
    (v2xy (.getLocalVector this (vec2 vect))))

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
       (position (body-of this)))
    ([this loc-pt]
       (position (body-of this) loc-pt)))
  (to-local [this pt]
    (to-local (body-of this) pt))
  (to-local-vect [this vect]
    (to-local-vect (body-of this) vect)))

(defn radius
  "Radius of a Fixture's shape."
  [^Fixture fixt]
  (.getRadius (.getShape fixt)))

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
                 (take n (map v2xy (.getVertices ^PolygonShape shp))))
      :edge [(v2xy (.m_vertex1 ^EdgeShape shp))
             (v2xy (.m_vertex2 ^EdgeShape shp))]
      :chain (let [n (.m_count ^ChainShape shp)]
               (take n (map v2xy (.m_vertices ^ChainShape shp)))))))

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

(defn linear-velocity-world
  "Linear velocity of a point on the body in world coordinates. In m/s."
  ([^Body body world-pt]
     (v2xy (.getLinearVelocityFromWorldPoint body (vec2 world-pt)))))

(defn angular-velocity
  "Angular velocity of a body in radians/second."
  [^Body body]
  (.getAngularVelocity body))

(defn apply-force!
  "Apply a force in Newtons to body at a world point. If the force is
   not applied at the center of mass, it will generate a torque and
   affect the angular velocity. This wakes up the body."
  [^Body body force pt]
  (.applyForce body (vec2 force) (vec2 pt)))

(defn apply-torque!
  "Apply a torque in N-m, i.e. about the z-axis (out of the screen).
   This affects the angular velocity without affecting the linear
   velocity of the center of mass. This wakes up the body."
  [^Body body torque]
  (.applyTorque body torque))

(defn apply-impulse!
  "Apply an impulse in N-seconds or kg-m/s at a point. This
   immediately modifies the velocity. It also modifies the angular
   velocity if the point of application is not at the center of mass.
   This wakes up the body."
  [^Body body impulse pt]
  (.applyLinearImpulse body (vec2 impulse) (vec2 pt)))

(defn linear-velocity!
  [^Body body vel]
  (.setLinearVelocity body (vec2 vel)))

(defn angular-velocity!
  [^Body body a-vel]
  (.setAngularVelocity body a-vel))

(defn gravity-scale
  [^Body body]
  (.getGravityScale body))

(defn gravity-scale!
  [^Body body z]
  (.setGravityScale body z))

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
  "Abstraction for JBox2D objects which can be destroyed."
  (destroy! [this]
    "Remove object from the World permanantly. Destroying a body
    automatically deletes all associated shapes and joints."))

(extend-protocol Destroyable
  Body
  (destroy! [this] (.destroyBody (.getWorld this) this))
  Joint
  (destroy! [this] (.destroyJoint (.getWorld (.getBodyA this)) this))
  Fixture
  (destroy! [this] (.destroyFixture (.getBody this) this)))

;; ### Spatial queries

(defn aabb
  "Axis-Aligned Bounding Box"
  ([[x0 y0] [x1 y1]]
     (AABB. (vec2 [(min x0 x1) (min y0 y1)])
            (vec2 [(max x0 x1) (max y0 y1)])))
  ([^Fixture fixt]
     (let [aabb* (AABB.)]
       (.computeAABB (.getShape fixt) aabb*
                     (.getTransform (.getBody fixt))
                     0)
       aabb*)))

(defn query-aabb
  "Return a vector of (up to a given number of) fixtures overlapping
   an Axis-Aligned Bounding Box"
  ([world ^AABB bb]
     (query-aabb bb 1000000))
  ([^World world ^AABB bb max-take]
     (let [fxx (atom [])
           cb (reify QueryCallback
                (reportFixture [_ fixt]
                  (swap! fxx conj fixt)
                  ;; return false to end search
                  (< (count @fxx) max-take)))]
       (.queryAABB world cb bb)
       @fxx)))

(defn query-at-point
  "Return a vector of fixtures overlapping the given point. The point
   is tested to be inside each shape, not just within its bounding
   box."
  ([world pt]
     (query-at-point world pt 1000000))
  ([^World world [x y] max-take]
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
       (.queryAABB world cb bb)
       @fxx)))

(defrecord RaycastContactData [fixture point normal fraction])

(defn raycast
  "Raycast from start-pt to end-pt, returning a sequence of fixture
   intersections, each a map (record) with keys `:fixture` `:point`
   `:normal` `:fraction`. If `mode` is `:all`, all intersections are
   returned in no particular order. If mode is `:closest`, only the
   closest intersection is returned in a single-element list, or an
   empty list if none exist. If an `:ignore` function is given,
   fixtures returning logical true from it are ignored."
   [^World world start-pt end-pt mode & {:keys [ignore]
                                         :or {ignore (constantly false)}}]
  (let [fxx (atom ())
        cb (reify RayCastCallback
             (reportFixture [_ fixt pt norm frac]
               (if (ignore fixt)
                 -1
                 (let [x (RaycastContactData. fixt (v2xy pt) (v2xy norm) frac)]
                   (case mode
                     :all (do (swap! fxx conj x)
                              1.0)
                     :closest (do (reset! fxx [x])
                                  frac))))))]
    (.raycast world cb (vec2 start-pt) (vec2 end-pt))
    @fxx))

;; ## Contacts

(defrecord ContactData [fixture-a fixture-b points normal])

(defn contact-data
  "Returns a map with keys :fixture-a :fixture-b :points :normal
   from a JBox2D Contact class. Returns nil if no contact points exist."
  [^Contact contact]
  (when (.isTouching contact)
    (let [world-manifold (WorldManifold.) ;; TODO could pass this in
          manifold (.getManifold contact)
          pcount (.pointCount manifold)]
      (when (pos? pcount)
        ;; mutates its argument:
        (.getWorldManifold contact world-manifold)
        (let [fixt-a (.getFixtureA contact)
              fixt-b (.getFixtureB contact)
              pts (map v2xy (take pcount (.points world-manifold)))
              normal (v2xy (.normal world-manifold))]
          (ContactData. fixt-a fixt-b pts normal))))))

(defn set-buffering-contact-listener!
  "Sets a ContactListener on the world which stores contacts. Returns
   an atom which will be populated with a sequence of `contact-data`
   records. Consumer is responsible for resetting it."
  [^World world]
  (let [contact-buffer (atom ())
        lstnr (reify ContactListener
                (beginContact [_ _])
                (endContact [_ _])
                (postSolve [_ _ _])
                (preSolve [_ contact _]
                  (if-let [cd (contact-data contact)]
                    (swap! contact-buffer conj cd))))]
    (.setContactListener world lstnr)
    contact-buffer))

(defn current-contacts
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
  (let [bodies (mapcat #(list (body-of (:fixture-a %))
                              (body-of (:fixture-b %)))
                       (current-contacts bod))]
    (set (remove #(= bod %) bodies))))

(defn all-current-contacts
  "Lazy seq of current contacts in the world. Each contact is a map as
   defined by the `contact-data` function. Contacts without actual
   contact points (i.e. created only due to overlapping bounding
   boxes) are excluded."
  [^World world]
  (letfn [(nextstep [^Contact cl]
            (when cl
              (if-let [cdata (contact-data cl)]
                (cons cdata (nextstep (.getNext cl)))
                (nextstep (.getNext cl)))))]
    (lazy-seq (nextstep (.getContactList world)))))

;; # Joints

;; ## Creation of joints

(defmulti joint!*
  :type)

(defmulti initspec
  "Augments the joint specification map to define body-local anchors,
   axes, etc. from given initial world values (e.g. `:world-anchor`)."
  :type)

(defn joint!
  "Creates a joint from a spec map according to its `:type` key. In
   most cases the map must contain `:body-a` and `:body-b`.

   * `:revolute` joint constrains two bodies to share a common point
     while they are free to rotate about the point. The relative
     rotation about the shared point is the joint angle. You can limit
     the relative rotation with a joint limit that specifies a lower
     and upper angle. You can use a motor to drive the relative
     rotation about the shared point. A maximum motor torque is
     provided so that infinite forces are not generated.

   * `:prismatic` joint. This requires defining a line of motion using
     an axis and an anchor point. The definition uses local anchor
     points and a local axis so that the initial configuration can
     violate the constraint slightly. The joint translation is zero
     when the local anchor points coincide in world space. Using local
     anchors and a local axis helps when saving and loading a game.

   * `:distance` joint. This requires defining an anchor point on both
     bodies and the non-zero length of the distance joint. The
     definition uses local anchor points so that the initial
     configuration can violate the constraint slightly. This helps
     when saving and loading a game. *Note* however that this
     initialisation function uses world points. For `:damping-ratio` 0
     = no damping; 1 = critical damping.`

   * `:rope` joint requires two body-local anchor points and a maximum
     length.

   * `:constant-volume` joint. Connects a group a bodies together so
     they maintain a constant volume within them. Uses Distance joints
     internally.

   * `:mouse` joint. By convention `body-a` is ground and `body-b` is
     the selection.

   * `:weld` joint. This requires defining a common anchor point on
     both bodies. This initialisation function takes a world point."
  [spec]
  (joint!* (initspec spec)))

(defmethod initspec :default
  [spec]
  spec)

(defmethod initspec :revolute
  [{:keys [body-a body-b world-anchor]
    :as spec}]
  (if (:anchor-a spec) ;; prefer local spec
    spec
    (assoc spec
      :anchor-a (to-local body-a world-anchor)
      :anchor-b (to-local body-b world-anchor)
      :reference-angle (- (angle body-b) (angle body-a)))))

(defmethod joint!* :revolute
  [{:keys [body-a body-b anchor-a anchor-b reference-angle
           enable-motor motor-speed max-motor-torque
           enable-limit lower-angle upper-angle
           collide-connected user-data]
    :or {reference-angle 0
         enable-motor false, motor-speed 0, max-motor-torque 10000,
         enable-limit false, lower-angle 0, upper-angle 360,
         collide-connected false}}]
  (let [jd (RevoluteJointDef.)]
    (set! (.bodyA jd) body-a)
    (set! (.bodyB jd) body-b)
    (.set (.localAnchorA jd) (vec2 anchor-a))
    (.set (.localAnchorB jd) (vec2 anchor-b))
    (set! (.referenceAngle jd) reference-angle)
    (set! (.enableMotor jd) enable-motor)
    (set! (.motorSpeed jd) motor-speed)
    (set! (.maxMotorTorque jd) max-motor-torque)
    (set! (.enableLimit jd) enable-limit)
    (set! (.lowerAngle jd) lower-angle)
    (set! (.upperAngle jd) upper-angle)
    (set! (.collideConnected jd) collide-connected)
    (set! (.userData jd) user-data)
    (.createJoint (.getWorld ^Body body-a) jd)))

(defmethod initspec :prismatic
  [{:keys [body-a body-b world-anchor world-axis]
    :as spec}]
  (if (:anchor-a spec) ;; prefer local spec
    spec
    (assoc spec
      :anchor-a (to-local body-a world-anchor)
      :anchor-b (to-local body-b world-anchor)
      :axis-a (v2xy (.getLocalVector ^Body body-a (vec2 world-axis)))
      :reference-angle (- (angle body-b) (angle body-a)))))

(defmethod joint!* :prismatic
  [{:keys [body-a body-b anchor-a anchor-b axis-a reference-angle
           enable-motor motor-speed max-motor-force
           enable-limit lower-trans upper-trans
           collide-connected user-data]
    :or {reference-angle 0
         enable-motor false, motor-speed 0, max-motor-force 10000,
         enable-limit false, lower-trans -10, upper-trans 10,
         collide-connected false}}]
  (let [jd (PrismaticJointDef.)]
    (set! (.bodyA jd) body-a)
    (set! (.bodyB jd) body-b)
    (.set (.localAnchorA jd) (vec2 anchor-a))
    (.set (.localAnchorB jd) (vec2 anchor-b))
    (.set (.localAxisA jd) (vec2 axis-a))
    (set! (.referenceAngle jd) reference-angle)
    (set! (.enableMotor jd) enable-motor)
    (set! (.motorSpeed jd) motor-speed)
    (set! (.maxMotorForce jd) max-motor-force)
    (set! (.enableLimit jd) enable-limit)
    (set! (.lowerTranslation jd) lower-trans)
    (set! (.upperTranslation jd) upper-trans)
    (set! (.collideConnected jd) collide-connected)
    (set! (.userData jd) user-data)
    (.createJoint (.getWorld ^Body body-a) jd)))

(defmethod initspec :distance
  [{:keys [body-a body-b world-anchor-a world-anchor-b]
    :as spec}]
  (if (:anchor-a spec) ;; prefer local spec
    spec
    (assoc spec
      :anchor-a (to-local body-a world-anchor-a)
      :anchor-b (to-local body-b world-anchor-b)
      :length (v-dist world-anchor-a world-anchor-b))))

(defmethod joint!* :distance
  [{:keys [body-a anchor-a body-b anchor-b length
           frequency-hz damping-ratio
           collide-connected user-data]
    :or {length 1.0, frequency-hz 0, damping-ratio 0,
         collide-connected false}}]
  (let [jd (DistanceJointDef.)]
    (set! (.bodyA jd) body-a)
    (set! (.bodyB jd) body-b)
    (.set (.localAnchorA jd) (vec2 anchor-a))
    (.set (.localAnchorB jd) (vec2 anchor-b))
    (set! (.length jd) length)
    (set! (.frequencyHz jd) frequency-hz)
    (set! (.dampingRatio jd) damping-ratio)
    (set! (.collideConnected jd) collide-connected)
    (set! (.userData jd) user-data)
    (.createJoint (.getWorld ^Body body-a) jd)))

(defmethod joint!* :rope
  [{:keys [body-a anchor-a body-b anchor-b max-length
           collide-connected user-data]
    :or {max-length 1.0
         collide-connected false}}]
  (let [jd (RopeJointDef.)]
    (set! (.bodyA jd) body-a)
    (set! (.bodyB jd) body-b)
    (.set (.localAnchorA jd) (vec2 anchor-a))
    (.set (.localAnchorB jd) (vec2 anchor-b))
    (set! (.maxLength jd) max-length)
    (set! (.collideConnected jd) collide-connected)
    (set! (.userData jd) user-data)
    (.createJoint (.getWorld ^Body body-a) jd)))

(defmethod joint!* :constant-volume
  [{:keys [bodies frequency-hz damping-ratio
           collide-connected user-data]
    :or {frequency-hz 0, damping-ratio 0,
         collide-connected false}}]
  (let [jd (ConstantVolumeJointDef.)]
    (doseq [body bodies]
      (.addBody jd body))
    (set! (.frequencyHz jd) frequency-hz)
    (set! (.dampingRatio jd) damping-ratio)
    (set! (.collideConnected jd) collide-connected)
    (set! (.userData jd) user-data)
    (.createJoint (.getWorld ^Body (first bodies)) jd)))

(defmethod joint!* :mouse
  [{:keys [body-a body-b target max-force
           frequency-hz damping-ratio
           collide-connected user-data]
    :or {max-force 1000,
         frequency-hz 5, damping-ratio 0.7,
         collide-connected true}}]
  (let [jd (MouseJointDef.)]
    (set! (.bodyA jd) body-a)
    (set! (.bodyB jd) body-b)
    (.set (.target jd) (vec2 target))
    (set! (.maxForce jd) max-force)
    (set! (.frequencyHz jd) frequency-hz)
    (set! (.dampingRatio jd) damping-ratio)
    (set! (.collideConnected jd) collide-connected)
    (set! (.userData jd) user-data)
    (.createJoint (.getWorld ^Body body-a) jd)))

(defmethod initspec :weld
  [{:keys [body-a body-b world-anchor]
    :as spec}]
  (if (:anchor-a spec) ;; prefer local spec
    spec
    (assoc spec
      :anchor-a (to-local body-a world-anchor)
      :anchor-b (to-local body-b world-anchor)
      :reference-angle (- (angle body-b) (angle body-a)))))

(defmethod joint!* :weld
  [{:keys [body-a body-b anchor-a anchor-b reference-angle
           collide-connected user-data]
    :or {reference-angle 0
         collide-connected false}}]
  (let [jd (WeldJointDef.)]
    (set! (.bodyA jd) body-a)
    (set! (.bodyB jd) body-b)
    (.set (.localAnchorA jd) (vec2 anchor-a))
    (.set (.localAnchorB jd) (vec2 anchor-b))
    (set! (.referenceAngle jd) reference-angle)
    (set! (.collideConnected jd) collide-connected)
    (set! (.userData jd) user-data)
    (.createJoint (.getWorld ^Body body-a) jd)))

;; ## Query of joints

(defn- jointseq*
  "Lazy seq of joints in a joint list"
  [^Joint joint]
  (lazy-seq (when joint (cons joint (jointseq* (.getNext joint))))))

(defn jointseq
  "Lazy seq of all joints connected to a body"
  [^Body body]
  (jointseq* (.getJointList body)))

(defn alljointseq
  "Lazy seq of all joints in the world."
  [^World world]
  (jointseq* (.getJointList world)))

(defn body-a
  "Return bodyA for a joint"
  [^Joint jt]
  (.getBodyA jt))

(defn body-b
  "Return bodyB for a joint"
  [^Joint jt]
  (.getBodyB jt))

(defn anchor-a
  "The anchor point on bodyA in world coordinates"
  [^Joint jt]
  ;; the method modifies its argument
  (let [v0 (vec2 [0 0])]
    (.getAnchorA jt v0)
    (v2xy v0)))

(defn anchor-b
  "The anchor point on bodyB in world coordinates"
  [^Joint jt]
  ;; the method modifies its argument
  (let [v0 (vec2 [0 0])]
    (.getAnchorB jt v0)
    (v2xy v0)))

(defn reaction-force
  "The reaction force on bodyB at the joint anchor in Newtons.
   Give the inverse of the timestep."
  [^Joint jt inv-dt]
  ;; the method modifies its argument
  (let [v0 (vec2 [0 0])]
    (.getReactionForce jt inv-dt v0)
    (v2xy v0)))

(defn reaction-torque
  "The reaction torque on bodyB in N*m.
   Give the inverse of the timestep."
  [^Joint jt inv-dt]
  (.getReactionTorque jt inv-dt))

;; TODO this is same as joint-speed which should not be in Motorised
(defn joint-angular-velocity
  "Relative angular velocity of two attached bodies in radians/second."
  [jt]
  (- (angular-velocity (body-b jt))
     (angular-velocity (body-a jt))))

(defn joint-angle
  [^RevoluteJoint jt]
  (in-pi-pi (.getJointAngle jt)))

;; ## Limits

;; There's no interface so let's impose a protocol.
;; One could call the java methods directly but these avoid reflection.

(defprotocol Limitable
  "Abstraction for JBox2D joints which can have limits"
  (limit-enabled? [this] "Whether limits are in effect on a joint.")
  (enable-limit! [this flag] "Set whether limits are in effect.")
  (limits [this] "Vector of limits on a joint.")
  (limits! [this limits] "Set [lower upper] limits on a joint."))

(extend-protocol Limitable

  RevoluteJoint
  (limit-enabled? [this] (.isLimitEnabled this))
  (enable-limit! [this flag] (.enableLimit this flag))
  (limits [this] [(.getLowerLimit this) (.getUpperLimit this)])
  (limits! [this limits] (.setLimits this (first limits) (second limits)))

  PrismaticJoint
  (limit-enabled? [this] (.isLimitEnabled this))
  (enable-limit! [this flag] (.enableLimit this flag))
  (limits [this] [(.getLowerLimit this) (.getUpperLimit this)])
  (limits! [this limits] (.setLimits this (first limits) (second limits))))

;; ## Motors

;; There's no interface so let's impose a protocol.
;; One could call the java methods directly but these avoid reflection.

(defprotocol Motorised
  "Abstraction for JBox2D joints which can have motors"
  (motor-enabled? [this] "Whether a motor is enabled on a joint.")
  (enable-motor! [this flag] "Set whether a motor is enabled.")
  (motor-speed [this] "Motor (target) speed, may or may not be enabled.")
  (motor-speed! [this speed] "Set motor (target) speed, may or may not be enabled.")
  (joint-speed [this] "Current joint speed, may be linear or angular.")
  (motor-force [this inv-dt] "Current motor force.")
  (motor-torque [this inv-dt] "Current motor torque.")
  (max-motor-force [this] "Maximum motor force")
  (max-motor-torque [this] "Maximum motor torque")
  (max-motor-force! [this force] "Set maximum motor force")
  (max-motor-torque! [this torque] "Set maximum motor torque"))

(extend-protocol Motorised

  RevoluteJoint
  (motor-enabled? [this] (.isMotorEnabled this))
  (enable-motor! [this flag] (.enableMotor this flag))
  (motor-speed [this] (.getMotorSpeed this))
  (motor-speed! [this speed] (.setMotorSpeed this speed))
  (joint-speed [this] (.getJointSpeed this))
  (motor-torque [this inv-dt] (.getMotorTorque this inv-dt))
  (max-motor-torque [this] (.getMaxMotorTorque this))
  (max-motor-torque! [this torque] (.setMaxMotorTorque this torque))

  PrismaticJoint
  (motor-enabled? [this] (.isMotorEnabled this))
  (enable-motor! [this flag] (.enableMotor this flag))
  (motor-speed [this] (.getMotorSpeed this))
  (motor-speed! [this speed] (.setMotorSpeed this speed))
  (joint-speed [this] (.getJointSpeed this))
  (motor-force [this inv-dt] (.getMotorForce this inv-dt))
  (max-motor-force [this] (.getMaxMotorForce this))
  (max-motor-force! [this force] (.setMaxMotorForce this force)))

(defn power-watts
  "Instantaneous rate of work done by a revolute joint in Watts.
   Multiply by the time step to get work in Joules."
  [jt]
  (* (joint-speed jt) (motor-torque jt 1.0)))

;; ## Snapshots - representing state as data for drawing

(defn body-moving?
  [body]
  (case (body-type body)
    :static false
    :kinematic true
    :dynamic (awake? body)))

(defn snapshot-fixture
  [fixt]
  (let [shp-type (shape-type fixt)
        basic-info {:user-data (user-data fixt)
                    :shape-type shp-type}]
    (if (= :circle shp-type)
      (assoc basic-info
        :radius (radius fixt)
        :center (loc-center fixt))
      (assoc basic-info
        :coords (local-coords fixt)))))

(defn snapshot-body
  [body motion-only?]
  (let [basic-info {:user-data (user-data body)}
        moving-info (assoc basic-info
                      :position (position body)
                      :angle (angle body))]
    (if motion-only?
      (if (body-moving? body)
        moving-info
        basic-info)
      ;; return full information
      (assoc moving-info
        :body-type (body-type body)
        :fixtures (->> (for [fixt (fixtureseq body)]
                         (snapshot-fixture fixt))
                       (doall))))))

(defn snapshot-joint
  [jt]
  (let [jt-type (joint-type jt)]
    {:joint-type jt-type
     :anchor-a (anchor-a jt)
     :anchor-b (anchor-b jt)
     :center-a (when (= :revolute jt-type)
                 (center (body-a jt)))
     :center-b (when (= :revolute jt-type)
                 (center (body-b jt)))}))

(defn- keep-stable
  "For use in merge-with, hopefully improving memory use by reusing
   existing data structures, in preference to equal re-generated
   ones."
  [old new]
  (if (= old new) old new))

(defn snapshot-scene
  "Returns an immutable value representation of the world for drawing,
   with keys :bodies and :joints. Body snapshots and joint snapshots
   are stored in two-level nested maps, allowing bodies to be grouped
   together. `identify` returns a 2-tuple giving the group
   and (nested) body keys. The default case stores all bodies under a
   single key `:all` and keys them by object hash value.

   If a non-nil `prev-scene` is given then it is used as a basis: only
   the differences are applied to form a new value, improving memory
   use via structural sharing.

   Argument `well-behaved?` asserts that Fixtures will not change, and
   that static bodies will not move: they can then be ignored for
   efficiency."
  ([world prev-scene well-behaved?]
     (snapshot-scene world prev-scene well-behaved?
                     (juxt (constantly :all) hash)))
  ([world prev-scene well-behaved? identify]
     (let [prev-bodies (:bodies prev-scene)]
       {:bodies
        (->> (bodyseq world)
             (reduce (fn [m body]
                       (let [id-path (identify body)]
                         (assoc-in m id-path
                                   (if-let [prev (get-in prev-bodies id-path)]
                                     (merge-with keep-stable
                                                 prev
                                                 (snapshot-body body well-behaved?))
                                     (snapshot-body body false)))))
                     {}))
        :joints
        (->> (alljointseq world)
             (reduce (fn [m jt]
                       (assoc-in m (identify jt)
                                 (snapshot-joint jt)))
                     {}))})))
