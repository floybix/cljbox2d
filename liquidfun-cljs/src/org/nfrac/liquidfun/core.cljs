(ns org.nfrac.liquidfun.core
  (:require [cljsjs.liquidfun]
            [org.nfrac.liquidfun.vec2d
             :refer [polar-xy v-add v-sub v-dist in-pi-pi PI TWOPI]]))

(defn vec2
  "Make a Box2D `Vec2` object from given `x`, `y`."
  ([[x y]] (js/b2Vec2. x y))
  ([x y] (js/b2Vec2. x y)))

(defn v2xy
  "Makes a vector [x y] from a Vec2"
  [v]
  [(.-x v) (.-y v)])

;; ## World

(defn new-world
  "Returns a new Box2D world. Gravity defaults to [0 -10] m/s^2.
  Because the js bindings refer to a global var 'world' that is
  assigned here as a side effect (yuck!). Awaiting
  https://github.com/google/liquidfun/pull/51"
  ([]
   (new-world [0 -10]))
  ([gravity]
   (let [w (js/b2World. (vec2 gravity))]
     (set! js/world w)
     w)))

(defn step!
  "Simulate the world for a time step given in seconds.
   Note that Box2D objects are locked during simulation.
   Returns the world (although it remains a mutable object)."
  ([world dt]
   (step! world dt 8 3))
  ([^World world dt velocity-iterations position-iterations]
   (.Step world dt velocity-iterations position-iterations)
   world))

;; ## Enums

(def ^{:private true}
  body-types
  {:dynamic js/b2_dynamicBody
   :static js/b2_staticBody
   :kinematic js/b2_kinematicBody})

(def ^{:private true}
  body-keywords
  (zipmap (vals body-types) (keys body-types)))

(defn body-type
  "The body type as a keyword `:dynamic` `:static` or `:kinematic`."
  [^Body body]
  (body-keywords (.GetType body)))

(def ^{:private true}
  shape-types
  {:circle js/b2Shape_Type_e_circle
   :polygon js/b2Shape_Type_e_polygon
   :edge js/b2Shape_Type_e_edge
   :chain js/b2Shape_Type_e_chain})

(def ^{:private true}
  shape-keywords
  (zipmap (vals shape-types) (keys shape-types)))

(defn shape-type
  "The shape type of a Fixture as a keyword e.g. `:circle` or `:polygon`."
  [^Fixture fixt]
  (shape-keywords (.-type (.-shape fixt))))

(def ^{:private true}
  joint-types
  {:unknown js/e_unknownJoint
   :revolute js/e_revoluteJoint
   :prismatic js/e_prismaticJoint
   :distance js/e_distanceJoint
   :pulley js/e_pulleyJoint
   :mouse js/e_mouseJoint
   :gear js/e_gearJoint
   :wheel js/e_wheelJoint
   :weld js/e_weldJoint
   :friction js/e_frictionJoint
   :rope js/e_ropeJoint
   :motor js/e_motorJoint})

(def ^{:private true}
  joint-keywords
  (zipmap (vals joint-types) (keys joint-types)))

(defn joint-type
  "The joint type as a keyword."
  [^Joint joint]
  :unknown
  #_
  (joint-keywords (.GetType joint)))

;; ## Creation of objects

;; ### Shapes

(defn circle
  "Create a circle shape, by default centered at [0 0]"
  ([radius]
   (circle radius [0 0]))
  ([radius center]
   (let [shape (js/b2CircleShape.)]
     (set! (.-radius shape) radius)
     (set! (.-position shape) (vec2 center))
     shape)))

(defn edge
  "Create an edge shape, a line between two points."
  [pt1 pt2]
  (let [shape (js/b2EdgeShape.)]
    (.Set shape (vec2 pt1) (vec2 pt2))
    shape))

(defn edge-chain
  "Creates a chain of edge shapes, assumed not self-intersecting, left
   open at the ends."
  [vertices]
  (let [shape (js/b2ChainShape.)
        va (apply array (map vec2 vertices))]
    (set! (.-vertices shape) va)
    shape))

(defn edge-loop
  "Creates a loop of edge shapes, assumed not self-intersecting, where
   the last point joins back to the first point."
  [vertices]
  (edge-chain (concat vertices (take 1 vertices))))

(defn box
  "Create a box shape from half-width, half-height,
   by default centered at [0 0]"
  ([hx hy]
   (let [shape (js/b2PolygonShape.)]
     (.SetAsBoxXY shape hx hy)
     shape))
  ([hx hy center]
   (box hx hy center 0))
  ([hx hy center angle]
   (let [shape (js/b2PolygonShape.)]
     (.SetAsBoxXYCenterAngle shape hx hy (vec2 center) angle)
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
  (let [shape (js/b2PolygonShape.)
        va (apply array (map vec2 vertices))]
    (set! (.-vertices shape) va)
    shape))

;; ### Userdata - like metadata

(defn user-data
  [this]
  ;(.GetUserData this)
  (.-user-data this))

(defn user-data!
  [this x]
  (set! (.-user-data this) x))

(defn vary-user-data
  [this f & args]
  (set! (.-user-data this) (apply f (user-data this) args)))

;; ### Fixtures

(defn fixture-def
  "A FixtureDef defines  ashape with some physical properties.
  Do not call this directly, instead use `(body!)` or `(fixture!)`.

  `:group-index` allows a certain group of objects to never
  collide (negative) or always collide (positive). Zero means no
  collision group."
  [{:keys [shape density friction restitution is-sensor
           group-index category-bits mask-bits
           user-data]
    :or {density 0.0, friction 0.2, restitution 0.0, is-sensor false
         group-index 0, category-bits 0x0001, mask-bits 0xFFFF}}]
  (let [fd (js/b2FixtureDef.)]
    (set! (.-shape fd) shape)
    (set! (.-density fd) density)
    (set! (.-friction fd) friction)
    (set! (.-restitution fd) restitution)
    (set! (.-isSensor fd) is-sensor)
    ;(set! (.-userData fd) user-data)
    (let [ff (.-filter fd)]
      (set! (.-groupIndex ff) group-index)
      (set! (.-categoryBits ff) category-bits)
      (set! (.-maskBits ff) mask-bits))
    fd))

(defn fixture!
  "Creates a Fixture on an existing Body. The second argument is a
   fixture specification map to be passed to the `fixture-def`
   function."
  [^b2Body body fixture-spec]
  (let [fx (.CreateFixtureFromDef body (fixture-def fixture-spec))]
    (when-let [ud (:user-data fixture-spec)]
      (user-data! fx ud))
    fx))

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
  (let [bd (js/b2BodyDef.)]
    (set! (.-type bd) (body-types type))
    (set! (.-position bd) (vec2 position))
    (set! (.-angle bd) angle)
    (set! (.-bullet bd) bullet)
    (set! (.-fixedRotation bd) fixed-rotation)
    (set! (.-gravityScale bd) gravity-scale)
    (set! (.-angularDamping bd) angular-damping)
    (set! (.-linearDamping bd) linear-damping)
    (set! (.-angularVelocity bd) angular-velocity)
    (set! (.-linearVelocity bd) (vec2 linear-velocity))
    ;(set! (.-userData bd) user-data)
    bd))

(defn body!
  "Creates a Body together with its Fixtures. The `body-spec` map is
   passed to the `body-def` function. Each of the `fixture-specs` are
   passed to the `fixture-def` function."
  [^World world body-spec & fixture-specs]
  (let [bd (body-def body-spec)
        bod (.CreateBody world bd)]
    (doseq [fspec fixture-specs]
      (fixture! bod fspec))
    (when-let [ud (:user-data body-spec)]
      (user-data! bod ud))
    bod))

;; ### Particle Systems

(def kw->particle-flag
  {:water js/b2_waterParticle ; Water particle.
   :zombie js/b2_zombieParticle ; Removed after next simulation step.
   :wall js/b2_wallParticle ; Zero velocity.
   :spring js/b2_springParticle ; With restitution from stretching.
   :elastic js/b2_elasticParticle ; With restitution from deformation.
   :viscous  js/b2_viscousParticle ; With viscosity.
   :powder js/b2_powderParticle ; Without isotropic pressure.
   :tensile js/b2_tensileParticle ; With surface tension.
   :color-mixing js/b2_colorMixingParticle ; Mix color between contacting particles.
   :destruction-listener js/b2_destructionListenerParticle ; Call b2DestructionListener on destruction.
   :barrier js/b2_barrierParticle ; Prevents other particles from leaking.
   :static-pressure js/b2_staticPressureParticle ; Less compressibility.
   :reactive js/b2_reactiveParticle ; Makes pairs or triads with other particles.
   :repulsive js/b2_repulsiveParticle ; With high repulsive force.
   ;; Call b2ContactListener when this particle is about to interact with
   ;; a rigid body or stops interacting with a rigid body.
   ;; This results in an expensive operation compared to using
   ;; b2_fixtureContactFilterParticle to detect collisions between
   ;; particles.
   :fixture-contact-listener js/b2_fixtureContactListenerParticle
   ;; Call b2ContactListener when this particle is about to interact with
   ;; another particle or stops interacting with another particle.
   ;; This results in an expensive operation compared to using
   ;; b2_particleContactFilterParticle to detect collisions between
   ;; particles.
   :particle-contact-listener js/b2_particleContactListenerParticle
   ;; Call b2ContactFilter when this particle interacts with rigid bodies.
   :fixture-contact-filter js/b2_fixtureContactFilterParticle
   ;; Call b2ContactFilter when this particle interacts with other
   ;; particles.
   :particle-contact-filter js/b2_particleContactFilterParticle})

(defn particle-flags
  [kw-set]
  (->> kw-set
       (map kw->particle-flag)
       (reduce bit-or 0)))

(defn particle-flag?
  [^long flag-val kw]
  (pos? (bit-and flag-val (kw->particle-flag kw))))

(def kw->particle-group-flag
  {:solid js/b2_solidParticleGroup
   :rigid js/b2_rigidParticleGroup
   :can-be-empty js/b2_particleGroupCanBeEmpty})

(defn particle-group-flags
  [kw-set]
  (->> kw-set
       (map kw->particle-group-flag)
       (reduce bit-or 0)))

(defn particle-color
  [[r g b a]]
  (js/b2ParticleColor. r g b a))

(defn particle-group-def
  [{:keys [angle angular-velocity color flags group group-flags
           lifetime linear-velocity position-data particle-count
           position shape strength stride]
    :or {angle 0
         angular-velocity 0
         color [0 0 0 0]
         lifetime 0.0
         linear-velocity [0 0]
         position-data nil
         particle-count 0
         position [0 0]
         shape nil
         strength 1
         stride 0}}]
  (let [pgd (js/b2ParticleGroupDef.)
        color* (particle-color color)]
    (when flags
      (set! (.-flags pgd) flags))
    (when group-flags
      (set! (.-groupFlags pgd) group-flags))
    (when group
      (set! (.-group pgd) group))
    (set! (.-angle pgd) angle)
    (set! (.-angularVelocity pgd) angular-velocity)
    (set! (.-color pgd) color*)
    (set! (.-lifetime pgd) lifetime)
    (set! (.-linearVelocity pgd) (vec2 linear-velocity))
    (set! (.-positionData pgd) position-data)
    (set! (.-particleCount pgd) particle-count)
    (set! (.-position pgd) (vec2 position))
    (set! (.-shape pgd) shape)
    (set! (.-strength pgd) strength)
    (set! (.-stride pgd) stride)
    pgd))

(defn particle-group!
  [ps pg-spec]
  (let [pg (.CreateParticleGroup ps (particle-group-def pg-spec))]
    (when-let [ud (:user-data pg-spec)]
      (user-data! pg ud))
    pg))

(defn particle-system-def
  "Do not call this directly, instead use `(particle-system!)`."
  [{:keys [color-mixing-strength damping-strength destroy-by-age
           ejection-strength elastic-strength lifetime-granularity
           powder-strength pressure-strength radius
           repulsive-strength spring-strength
           static-pressure-iterations static-pressure-relaxation
           static-pressure-strength surface-tension-normal-strength
           surface-tension-pressure-strength viscous-strength]
    :or {color-mixing-strength 0.5
         damping-strength 1.0
         destroy-by-age true
         ejection-strength 0.5
         elastic-strength 0.25
         lifetime-granularity (/ 1.0 60.0)
         powder-strength 0.5
         pressure-strength 0.05
         radius 1.0
         repulsive-strength 1.0
         spring-strength 0.25
         static-pressure-iterations 8
         static-pressure-relaxation 0.2
         static-pressure-strength 0.2
         surface-tension-normal-strength 0.2
         surface-tension-pressure-strength 0.2
         viscous-strength 0.25}}]
  (let [psd (js/b2ParticleSystemDef.)]
    (set! (.-colorMixingStrength psd) color-mixing-strength)
    (set! (.-dampingStrength psd) damping-strength)
    (set! (.-destroyByAge psd) destroy-by-age)
    (set! (.-ejectionStrength psd) ejection-strength)
    (set! (.-elasticStrength psd) elastic-strength)
    (set! (.-lifetimeGranularity psd) lifetime-granularity)
    (set! (.-powderStrength psd) powder-strength)
    (set! (.-pressureStrength psd) pressure-strength)
    (set! (.-radius psd) radius)
    (set! (.-repulsiveStrength psd) repulsive-strength)
    (set! (.-springStrength psd) spring-strength)
    (set! (.-staticPressureIterations psd) static-pressure-iterations)
    (set! (.-staticPressureRelaxation psd) static-pressure-relaxation)
    (set! (.-staticPressureStrength psd) static-pressure-strength)
    (set! (.-surfaceTensionNormalStrength psd) surface-tension-normal-strength)
    (set! (.-surfaceTensionPressureStrength psd) surface-tension-pressure-strength)
    (set! (.-viscousStrength psd) viscous-strength)
    psd))

(defn particle-system!
  "Creates a Particle System with its groups. The `psd-spec` map is
   passed to the `particle-system-def` function. Each of the
   `group-specs` are passed to the `particle-group-def` function."
  [^World world psd-spec & group-specs]
  (let [psd (particle-system-def psd-spec)
        ps (.CreateParticleSystem world psd)]
    (doseq [gspec group-specs]
      (particle-group! ps gspec))
    (when-let [ud (:user-data psd-spec)]
      (user-data! ps ud))
    ps))

(defn particle-sys-seq
  [world]
  (seq (.-particleSystems world)))

(defn particle-positions
  [particle-system]
  (let [b (.GetPositionBuffer particle-system)]
    (partition 2 2 (es6-iterator-seq (.values b)))))

(defn particle-colors
  [particle-system]
  (let [b (.GetColorBuffer particle-system)]
    (partition 4 4 (es6-iterator-seq (.values b)))))

(defn particle-group-seq
  [ps]
  (seq (.-particleGroups ps)))

;; TODO missing
#_
(defn destroy-particle!
  [ps i]
  (.DestroyParticle ps i))

(defn particle-def
  [{:keys [color flags group lifetime velocity position]
    :or {lifetime 0.0
         position [0 0]}}]
  (let [pd (js/b2ParticleDef.)
        [x y] position]
    (when color
      (set! (.-color pd) (particle-color color)))
    (when flags
      (set! (.-flags pd) flags))
    (when group
      (set! (.-group pd) group))
    (set! (.-lifetime pd) lifetime)
    (set! (.-velocity pd) (vec2 velocity))
    (set! (.-position pd) (vec2 position))
    pd))

(defn particle!
  [ps particle-def]
  (.CreateParticle ps particle-def))

;; ## Query of objects

(defn ^Body body-of
  "Get the body to which a fixture belongs"
  [^Fixture fixt]
  (.-body fixt))

(defn bodyseq
  "Lazy seq of all bodies in the world"
  ([^World world]
   (seq (.-bodies world))))

(defn fixtureseq
  "Lazy seq of fixtures on a body."
  [^Body body]
  (seq (.-fixtures body)))

(defn fixture-of
  "Often a body will only have one fixture. This is a convenience
   function to pull out the first fixture from a body."
  [^Body body]
  {:pre [(= 1 (count (fixtureseq body)))]}
  (first (fixtureseq body)))

;; ### Coordinates

(defn mass
  "Total mass in kg."
  [this]
  (.GetMass this))

(defn center
  "Center of mass in world coordinates."
  [this]
  (v2xy (.GetWorldCenter this)))

(defn to-local
  "Local coordinates of a world point."
  [this pt]
  (v2xy (.GetLocalPoint this (vec2 pt))))

(defn to-local-vect
  "Local vector of a world vector."
  [this vect]
  (v2xy (.GetLocalVector this (vec2 vect))))

(defn loc-center
  "Center of mass in local coordinates."
  [this]
  (to-local this (center this)))

(defn position
  "World coordinates of a local point, default [0 0]."
  ([this]
   (v2xy (.GetPosition this)))
  ([this loc-pt]
   (v2xy (.GetWorldPoint this (vec2 loc-pt)))))

(defn circle-loc-center
  [fixt]
  (v2xy (.-position (.-shape fixt))))

(defn fixture-radius
  "Radius of a Fixture's shape."
  [^Fixture fixt]
  (.-radius (.-shape fixt)))

(defn local-coords
  "Local coordinates of polygon vertices. Approximated for circles."
  [^Fixture fixt]
  (let [shp (.-shape fixt)]
    (case (shape-type fixt)
      :circle (let [r (fixture-radius fixt)
                    cent (circle-loc-center fixt)]
                (for [a (range (- PI) PI (/ TWOPI 30))]
                  (v-add cent (polar-xy r a))))
      :polygon (mapv v2xy (.-vertices shp))
      :edge [(v2xy (.-vertex1 shp))
             (v2xy (.-vertex2 shp))]
      :chain (mapv v2xy (.-vertices shp)))))

(defn world-coords
  "World coordinates of polygon vertices. Approximated for circles."
  [^Fixture fixt]
  (let [body (.-body fixt)]
    (mapv #(position body %) (local-coords fixt))))

(defn angle
  "Angle of a body in radians"
  [^Body body]
  (in-pi-pi (.GetAngle body)))

;; ## Movement

(defn linear-velocity
  "Linear velocity of a point on the body in local coordinates, by
   default its center of mass. In m/s."
  ([^Body body]
   (v2xy (.GetLinearVelocity body)))
  ([^Body body loc-pt]
   ;; TODO missing
   (v2xy (.getLinearVelocityFromLocalPoint body (vec2 loc-pt)))))

(defn linear-velocity-world
  "Linear velocity of a point on the body in world coordinates. In m/s."
  ([^Body body world-pt]
   ;; TODO missing
   (v2xy (.getLinearVelocityFromWorldPoint body (vec2 world-pt)))))

(defn angular-velocity
  "Angular velocity of a body in radians/second."
  [^Body body]
  (.GetAngularVelocity body))

(defn apply-force!
  "Apply a force in Newtons to body at a world point. If the force is
   not applied at the center of mass, it will generate a torque and
   affect the angular velocity. This wakes up the body."
  [^Body body force pt]
  (.ApplyForce body (vec2 force) (vec2 pt) true))

(defn apply-torque!
  "Apply a torque in N-m, i.e. about the z-axis (out of the screen).
   This affects the angular velocity without affecting the linear
   velocity of the center of mass. This wakes up the body."
  [^Body body torque]
  (.ApplyTorque body torque true))

(defn apply-impulse!
  "Apply an impulse in N-seconds or kg-m/s at a point. This
   immediately modifies the velocity. It also modifies the angular
   velocity if the point of application is not at the center of mass.
   This wakes up the body."
  [^Body body impulse pt]
  (.ApplyLinearImpulse body (vec2 impulse) (vec2 pt) true))

(defn linear-velocity!
  [^Body body vel]
  (.SetLinearVelocity body (vec2 vel)))

(defn angular-velocity!
  [^Body body a-vel]
  (.SetAngularVelocity body a-vel))

;; TODO missing
(defn awake?
  [^Body body]
  true #_(.IsAwake body))

(defn wake!
  "Wake up a body."
  [^Body body]
  (.SetAwake body true))

(defn sleep!
  "Put a body to sleep."
  [^Body body]
  (.SetAwake body false))

(defn destroy-body!
  "Remove object from the World permanantly. Destroying a body
  automatically deletes all associated shapes and joints."
  [this]
  (.DestroyBody js/world this))

(defn destroy-joint!
  [this]
  (.DestroyJoint js/world this))

(defn destroy-fixture!
  [this]
  (.DestroyFixture (body-of this) this))

(defn destroy-particle-system!
  [this]
  (.DestroyParticleSystem js/world this))

;; ### Spatial queries

(defn aabb
  "Axis-Aligned Bounding Box"
  ([[x0 y0] [x1 y1]]
   (let [a (js/b2AABB.)]
     (set! (.-lowerBound a) (vec2 [(min x0 x1) (min y0 y1)]))
     (set! (.-upperBound a) (vec2 [(max x0 x1) (max y0 y1)]))
     a))
  ([^Fixture fixt]
   (let [cc (world-coords fixt)
         xs (map first cc)
         ys (map second cc)]
     (aabb [(reduce min xs) (reduce min ys)]
           [(reduce max xs) (reduce max ys)]))))

(defn report-fixture-callback
  [f]
  (let [cb (js-obj)]
    (set! (.-ReportFixture cb) f)
    cb))

(defn query-aabb
  "Return a vector of (up to a given number of) fixtures overlapping
   an Axis-Aligned Bounding Box"
  ([world ^AABB bb]
   (query-aabb bb 1000000))
  ([^World world ^AABB bb max-take]
   (let [fxx (atom [])
         cb (report-fixture-callback
             (fn [fixt]
               (swap! fxx conj fixt)
               ;; return false to end search
               (< (count @fxx) max-take)))]
     (.QueryAABB world cb bb)
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
         cb (report-fixture-callback
             (fn [fixt]
               (if (.TestPoint fixt pt-vec2)
                 (swap! fxx conj fixt))
               ;; return false to end search
               (< (count @fxx) max-take)))]
     (.QueryAABB world cb bb)
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
        cb (report-fixture-callback
             (fn [fixt pt norm frac]
               (if (ignore fixt)
                 -1
                 (let [x (RaycastContactData. fixt (v2xy pt) (v2xy norm) frac)]
                   (case mode
                     :all (do (swap! fxx conj x)
                              1.0)
                     :closest (do (reset! fxx [x])
                                  frac))))))]
    (.RayCast world cb (vec2 start-pt) (vec2 end-pt))
    @fxx))

;; ## Contacts

(defrecord ContactData [fixture-a fixture-b points normal
                        normal-impulses tangent-impulses])

(let [b2-normal-offset (-> js/Offsets (.-b2WorldManifold) (.-normal))]

  (defn- manifold-normal
    [manifold]
    (-> manifold (.-buffer) (.getFloat32 b2-normal-offset true))))

(defn contact-data
  "Returns a map with keys :fixture-a :fixture-b :points :normal
   :normal-impulses :tangent-impulses from a JBox2D Contact object and
   optional ContactImpulse object. Returns nil if no contact points
   exist."
  ([^Contact contact]
   (contact-data contact nil))
  ([^Contact contact ^ContactImpulse impulses]
   (when true #_(.isTouching contact)
     (let [manifold (.GetManifold contact)
           pcount (.GetPointCount manifold)]
       (when (pos? pcount)
         (let [world-manifold (.GetWorldManifold contact)
               fixt-a (.GetFixtureA contact)
               fixt-b (.GetFixtureB contact)
               pts (mapv (fn [i] (v2xy (.GetPoint world-manifold i)))
                         (range pcount))
               normal (v2xy (manifold-normal world-manifold))]
           (ContactData. fixt-a fixt-b pts normal
                         (when impulses
                           (mapv #(.GetNormalImpulse impulses %) (range pcount)))
                         (when impulses
                           (mapv #(.GetTangentImpulse impulses %) (range pcount))))))))))

(defn contact-listener-callback
  [{:keys [begin-contact end-contact presolve postsolve]}]
  (let [cb (js-obj)]
    (when begin-contact
      (set! (.-BeginContactBody cb) begin-contact))
    (when end-contact
      (set! (.-EndContactBody cb) end-contact))
    (when presolve
      (set! (.-PreSolve cb) presolve))
    (when postsolve
      (set! (.-PostSolve cb) postsolve))
    cb))

(defn set-buffering-contact-listener!
  "Sets a ContactListener on the world which stores contacts. Returns
   an atom which will be populated with a sequence of `contact-data`
   records. Consumer is responsible for emptying it."
  [^World world]
  (let [contact-buffer (atom ())
        lstnr (contact-listener-callback
                {:postsolve
                 (fn [contact impulses]
                  (if-let [cd (contact-data contact impulses)]
                    (swap! contact-buffer conj cd)))})]
    (.SetContactListener world lstnr)
    contact-buffer))

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

   * `:mouse` joint. By convention `body-a` is ground and `body-b` is
     the selection.

   * `:weld` joint. This requires defining a common anchor point on
     both bodies. This initialisation function takes a world point."
  [spec]
  (let [jt (joint!* (initspec spec))]
    (when-let [ud (:user-data spec)]
      (user-data! jt ud))
    jt))

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
         enable-limit false, lower-angle 0, upper-angle TWOPI,
         collide-connected false}}]
  (let [jd (js/b2RevoluteJointDef.)]
    (set! (.-bodyA jd) body-a)
    (set! (.-bodyB jd) body-b)
    (set! (.-localAnchorA jd) (vec2 anchor-a))
    (set! (.-localAnchorB jd) (vec2 anchor-b))
    (set! (.-referenceAngle jd) reference-angle)
    (set! (.-enableMotor jd) enable-motor)
    (set! (.-motorSpeed jd) motor-speed)
    (set! (.-maxMotorTorque jd) max-motor-torque)
    (set! (.-enableLimit jd) enable-limit)
    (set! (.-lowerAngle jd) lower-angle)
    (set! (.-upperAngle jd) upper-angle)
    (set! (.-collideConnected jd) collide-connected)
    ;(set! (.-userData jd) user-data)
    (.CreateJoint js/world #_(.GetWorld body-a) jd)))

(defmethod initspec :prismatic
  [{:keys [body-a body-b world-anchor world-axis]
    :as spec}]
  (if (:anchor-a spec) ;; prefer local spec
    spec
    (assoc spec
      :anchor-a (to-local body-a world-anchor)
      :anchor-b (to-local body-b world-anchor)
      :axis-a (to-local-vect body-a world-axis)
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
  (let [jd (js/b2PrismaticJointDef.)]
    (set! (.-bodyA jd) body-a)
    (set! (.-bodyB jd) body-b)
    (set! (.-localAnchorA jd) (vec2 anchor-a))
    (set! (.-localAnchorB jd) (vec2 anchor-b))
    (set! (.-localAxisA jd) (vec2 axis-a))
    (set! (.-referenceAngle jd) reference-angle)
    (set! (.-enableMotor jd) enable-motor)
    (set! (.-motorSpeed jd) motor-speed)
    (set! (.-maxMotorForce jd) max-motor-force)
    (set! (.-enableLimit jd) enable-limit)
    (set! (.-lowerTranslation jd) lower-trans)
    (set! (.-upperTranslation jd) upper-trans)
    (set! (.-collideConnected jd) collide-connected)
    ;(set! (.-userData jd) user-data)
    (.CreateJoint js/world #_(.GetWorld body-a) jd)))

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
  (let [jd (js/b2DistanceJointDef.)]
    (set! (.-bodyA jd) body-a)
    (set! (.-bodyB jd) body-b)
    (set! (.-localAnchorA jd) (vec2 anchor-a))
    (set! (.-localAnchorB jd) (vec2 anchor-b))
    (set! (.-length jd) length)
    (set! (.-frequencyHz jd) frequency-hz)
    (set! (.-dampingRatio jd) damping-ratio)
    (set! (.-collideConnected jd) collide-connected)
    ;(set! (.-userData jd) user-data)
    (.CreateJoint js/world #_(.GetWorld body-a) jd)))

(defmethod joint!* :rope
  [{:keys [body-a anchor-a body-b anchor-b max-length
           collide-connected user-data]
    :or {max-length 1.0
         collide-connected false}}]
  (let [jd (js/b2RopeJointDef.)]
    (set! (.-bodyA jd) body-a)
    (set! (.-bodyB jd) body-b)
    (set! (.-localAnchorA jd) (vec2 anchor-a))
    (set! (.-localAnchorB jd) (vec2 anchor-b))
    (set! (.-maxLength jd) max-length)
    (set! (.-collideConnected jd) collide-connected)
    ;(set! (.-userData jd) user-data)
    (.CreateJoint js/world #_(.GetWorld body-a) jd)))

(defmethod joint!* :mouse
  [{:keys [body-a body-b target max-force
           frequency-hz damping-ratio
           collide-connected user-data]
    :or {max-force 1000,
         frequency-hz 5, damping-ratio 0.7,
         collide-connected true}}]
  (let [jd (js/b2MouseJointDef.)]
    (set! (.-bodyA jd) body-a)
    (set! (.-bodyB jd) body-b)
    (set! (.-target jd) (vec2 target))
    (set! (.-maxForce jd) max-force)
    (set! (.-frequencyHz jd) frequency-hz)
    (set! (.-dampingRatio jd) damping-ratio)
    (set! (.-collideConnected jd) collide-connected)
    ;(set! (.-userData jd) user-data)
    (.CreateJoint js/world #_(.GetWorld body-a) jd)))

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
  (let [jd (js/b2WeldJointDef.)]
    (set! (.-bodyA jd) body-a)
    (set! (.-bodyB jd) body-b)
    (set! (.-localAnchorA jd) (vec2 anchor-a))
    (set! (.-localAnchorB jd) (vec2 anchor-b))
    (set! (.-referenceAngle jd) reference-angle)
    (set! (.-collideConnected jd) collide-connected)
    ;(set! (.-userData jd) user-data)
    (.CreateJoint js/world #_(.GetWorld body-a) jd)))

;; ## Query of joints

(defn alljointseq
  "Lazy seq of all joints in the world."
  [^World world]
  (seq (.-joints world)))

(defn jointseq
  "Lazy seq of all joints connected to a body"
  [^Body body]
  (filter #(or (= body (.GetBodyA %))
               (= body (.GetBodyB %)))
          (alljointseq js/world #_(.GetWorld body))))

(defn body-a
  "Return bodyA for a joint"
  [^Joint jt]
  (or (.-bodyA jt) (.GetBodyA jt)))

(defn body-b
  "Return bodyB for a joint"
  [^Joint jt]
  (or (.-bodyB jt) (.GetBodyB jt)))

(defn anchor-a
  "The anchor point on bodyA in world coordinates"
  [^Joint jt]
  ;; TODO dummy!
  #_(.GetAnchorA jt)
  (position (body-a jt)))

(defn anchor-b
  "The anchor point on bodyB in world coordinates"
  [^Joint jt]
  ;; TODO dummy!
  #_(.GetAnchorB jt)
  (position (body-b jt)))

;;;; some monkeypatching
;(set! (.. js/b2DistanceJoint -prototype -GetBodyA) (fn [this] (.-bodyA this)))
;(set! (.. js/b2DistanceJoint -prototype -GetBodyB) (fn [this] (.-bodyB this)))

#_
(defn reaction-force
  "The reaction force on bodyB at the joint anchor in Newtons.
   Give the inverse of the timestep."
  [^Joint jt inv-dt]
  ;; the method modifies its argument
  (let [v0 (vec2 [0 0])]
    (.getReactionForce jt inv-dt v0)
    (v2xy v0)))

#_
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
  (in-pi-pi (.GetJointAngle jt)))

(defn joint-translation
  [jt]
  (.GetJointTranslation jt))

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

  js/b2RevoluteJoint
  (limit-enabled? [this] (pos? (.IsLimitEnabled this)))
  (enable-limit! [this flag] (.EnableLimit this flag))
  #_(limits [this] [(.getLowerLimit this) (.getUpperLimit this)])
  #_(limits! [this limits] (.setLimits this (first limits) (second limits)))

  js/b2PrismaticJoint
  (limit-enabled? [this] (pos? (.IsLimitEnabled this)))
  (enable-limit! [this flag] (.EnableLimit this flag))
  #_(limits [this] [(.getLowerLimit this) (.getUpperLimit this)])
  #_(limits! [this limits] (.setLimits this (first limits) (second limits))))

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

  js/b2RevoluteJoint
  (motor-enabled? [this] (pos? (.IsMotorEnabled this)))
  (enable-motor! [this flag] (.EnableMotor this flag))
  (motor-speed [this] (.GetMotorSpeed this))
  (motor-speed! [this speed] (.SetMotorSpeed this speed))
  ;(joint-speed [this] (.getJointSpeed this))
  ;(motor-torque [this inv-dt] (.getMotorTorque this inv-dt))
  ;(max-motor-torque [this] (.getMaxMotorTorque this))
  ;(max-motor-torque! [this torque] (.setMaxMotorTorque this torque))

  js/b2PrismaticJoint
  (motor-enabled? [this] (pos? (.IsMotorEnabled this)))
  (enable-motor! [this flag] (.EnableMotor this flag))
  (motor-speed [this] (.GetMotorSpeed this))
  (motor-speed! [this speed] (.SetMotorSpeed this speed))
  ;(joint-speed [this] (.getJointSpeed this))
  (motor-force [this inv-dt] (.GetMotorForce this inv-dt)))
  ;(max-motor-force [this] (.getMaxMotorForce this))
  ;(max-motor-force! [this force] (.setMaxMotorForce this force)))

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
        :radius (fixture-radius fixt)
        :center (circle-loc-center fixt))
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
      ()
      #_
      (->> (alljointseq world)
           (reduce (fn [m jt]
                     (assoc-in m (identify jt)
                               (snapshot-joint jt)))
                   {}))})))
