(ns org.nfrac.liquidfun.core
  (:require [org.nfrac.liquidfun.vec2d
             :refer [polar-xy v-add v-sub v-dist in-pi-pi PI TWOPI]])
  (:import (org.bytedeco.javacpp
            IntPointer
            FloatPointer
            liquidfun
            liquidfun$b2Vec2
            liquidfun$b2World
            liquidfun$b2Body
            liquidfun$b2BodyDef
            liquidfun$b2Fixture
            liquidfun$b2FixtureDef
            liquidfun$b2Shape
            liquidfun$b2PolygonShape
            liquidfun$b2CircleShape
            liquidfun$b2EdgeShape
            liquidfun$b2ChainShape
            liquidfun$b2Joint
            liquidfun$b2RevoluteJoint
            liquidfun$b2PrismaticJoint
            liquidfun$b2DistanceJoint
            liquidfun$b2PulleyJoint
            liquidfun$b2MouseJoint
            liquidfun$b2GearJoint
            liquidfun$b2WheelJoint
            liquidfun$b2WeldJoint
            liquidfun$b2FrictionJoint
            liquidfun$b2RopeJoint
            liquidfun$b2MotorJoint
            liquidfun$b2JointDef
            liquidfun$b2Contact
            liquidfun$b2ContactImpulse
            liquidfun$b2ContactEdge
            liquidfun$b2ContactListener
            liquidfun$b2DestructionListener
            liquidfun$b2AABB
            liquidfun$b2QueryCallback
            liquidfun$b2RayCastCallback
            liquidfun$b2WorldManifold
            liquidfun$b2ParticleSystem
            liquidfun$b2ParticleSystemDef
            liquidfun$b2ParticleGroup
            liquidfun$b2ParticleGroupDef
            liquidfun$b2ParticleDef)))

(defmacro $b2
  "Expands to constructor form (liquidfun$b2Class. ...)"
  [Class & args]
  (let [fq (symbol (str "org.bytedeco.javacpp.liquidfun$b2" Class))]
    (list* fq args)))

(defn ptr->floats
  [^FloatPointer p n]
  (let [a (float-array n)]
    (.get p a 0 n)
    (seq a)))

(defn ptr->ints
  [^IntPointer p n]
  (let [a (int-array n)]
    (.get p a 0 n)
    (seq a)))

(defn vec2
  "Make a Box2D Vec2 object from given `x`, `y`."
  (^liquidfun$b2Vec2 [[x y]] ($b2 Vec2. x y))
  (^liquidfun$b2Vec2 [x y] ($b2 Vec2. x y)))

(defn v2xy
  "Makes an immutable vector [x y] from a Vec2"
  [^liquidfun$b2Vec2 v2]
  [(.x v2) (.y v2)])

(defn seq->v2arr
  "Constructs a native c++ array of Vec2s from a seq of [x y]."
  [vs]
  (let [n (count vs)
        ;; the JavaCPP native array allocator:
        va ($b2 Vec2. n)]
    (loop [i 0
           vs (seq vs)]
      (if (< i n)
        (let [[x y] (first vs)]
          (.Set (.position va i) x y)
          (recur (inc i) (rest vs)))
        (.position va 0)))))

(defn v2arr->seq
  "Makes a seq of [x y] from a native c++ array of n Vec2s."
  [^liquidfun$b2Vec2 v2arr n]
  (loop [i 0
         va v2arr
         ans ()]
    (if (< i n)
      (let [vi (v2xy va)]
        (recur (inc i) (.position va (inc i)) (conj ans vi)))
      (do
        (.position v2arr 0)
        ans))))

;; ## World

(defn new-world
  "Returns a new Box2D world. Gravity defaults to [0 -10] m/s^2."
  (^liquidfun$b2World []
   (new-world [0 -10]))
  (^liquidfun$b2World [gravity]
   (let [[x y] gravity]
     ($b2 World. x y))))

(defn step!
  "Simulate the world for a time step given in seconds.
   Note that Box2D objects are locked during simulation.
   Returns the world (although it remains a mutable object)."
  ([world dt]
   (step! world dt 8 3))
  ([^liquidfun$b2World world dt velocity-iterations position-iterations]
   (.Step world dt velocity-iterations position-iterations)
   world)
  ([^liquidfun$b2World world dt velocity-iterations position-iterations
    particle-iterations]
   (.Step world dt velocity-iterations position-iterations particle-iterations)
   world))

;; ## Enums

(def ^{:private true}
  body-types
  {:dynamic liquidfun/b2_dynamicBody
   :static liquidfun/b2_staticBody
   :kinematic liquidfun/b2_kinematicBody})

(def ^{:private true}
  body-keywords
  (zipmap (vals body-types) (keys body-types)))

(defn body-type
  "The body type as a keyword `:dynamic` `:static` or `:kinematic`."
  [^liquidfun$b2Body body]
  (body-keywords (.GetType body)))

(def ^{:private true}
  shape-types
  {:circle liquidfun$b2Shape/e_circle
   :polygon liquidfun$b2Shape/e_polygon
   :edge liquidfun$b2Shape/e_edge
   :chain liquidfun$b2Shape/e_chain})

(def ^{:private true}
  shape-keywords
  (zipmap (vals shape-types) (keys shape-types)))

(defn shape-type
  "The shape type of a Fixture as a keyword e.g. `:circle` or `:polygon`."
  [^liquidfun$b2Fixture fixt]
  (shape-keywords (.GetType (.GetShape fixt))))

(def ^{:private true}
  joint-types
  {:unknown liquidfun/e_unknownJoint
   :revolute liquidfun/e_revoluteJoint
   :prismatic liquidfun/e_prismaticJoint
   :distance liquidfun/e_distanceJoint
   :pulley liquidfun/e_pulleyJoint
   :mouse liquidfun/e_mouseJoint
   :gear liquidfun/e_gearJoint
   :wheel liquidfun/e_wheelJoint
   :weld liquidfun/e_weldJoint
   :friction liquidfun/e_frictionJoint
   :rope liquidfun/e_ropeJoint
   :motor liquidfun/e_motorJoint})

(def ^{:private true}
  joint-keywords
  (zipmap (vals joint-types) (keys joint-types)))

(defn joint-type
  "The joint type as a keyword."
  [^liquidfun$b2Joint joint]
  (joint-keywords (.GetType joint)))

(defn classy-joint
  [p]
  (case (joint-type p)
    :revolute ($b2 RevoluteJoint. p)
    :prismatic ($b2 PrismaticJoint. p)
    :distance ($b2 DistanceJoint. p)
    :pulley ($b2 PulleyJoint. p)
    :mouse ($b2 MouseJoint. p)
    :gear ($b2 GearJoint. p)
    :wheel ($b2 WheelJoint. p)
    :weld ($b2 WeldJoint. p)
    :friction ($b2 FrictionJoint. p)
    :rope ($b2 RopeJoint. p)
    :motor ($b2 MotorJoint. p)
    p))

;; ## Creation of objects

;; ### Shapes

(defn circle
  "Create a circle shape, by default centered at [0 0]"
  ([radius]
   (circle radius [0 0]))
  ([radius center]
   (let [shp ($b2 CircleShape.)
         [x y] center]
     (.m_radius shp radius)
     (.SetPosition shp x y)
     shp)))

(defn edge
  "Create an edge shape, a line between two points."
  [pt1 pt2]
  (let [shp ($b2 EdgeShape.)
        [x1 y1] pt1
        [x2 y2] pt2]
    (.Set shp x1 y1 x2 y2)
    shp))

(defn edge-chain
  "Creates a chain of edge shapes, assumed not self-intersecting, left
   open at the ends."
  [vertices]
  (let [shape ($b2 ChainShape.)
        va (seq->v2arr vertices)]
    (.CreateChain shape va (count vertices))
    shape))

(defn edge-loop
  "Creates a loop of edge shapes, assumed not self-intersecting, where
   the last point joins back to the first point."
  [vertices]
  (let [shape ($b2 ChainShape.)
        va (seq->v2arr vertices)]
    (.CreateLoop shape va (count vertices))
    shape))

(defn box
  "Create a box shape from half-width, half-height,
   by default centered at [0 0]"
  ([hx hy]
   (let [shape ($b2 PolygonShape.)]
     (.SetAsBox shape hx hy)
     shape))
  ([hx hy center]
   (box hx hy center 0))
  ([hx hy center angle]
   (let [shape ($b2 PolygonShape.)
         [x y] center]
     (.SetAsBox shape hx hy x y angle)
     shape)))

(defn rod
  "Create a long thin (box) shape extending from a point to a given
   length and with a given angle."
  [from-pt angle length width]
  (box (/ length 2) (/ width 2)
       (v-add from-pt (polar-xy (/ length 2) angle))
       angle))

(defn polygon
  "Create a polygon shape as the convex hull over a set of points."
  [vertices]
  (let [shape ($b2 PolygonShape.)
        va (seq->v2arr vertices)]
    (.Set shape va (count vertices))
    shape))

;; ### Userdata - holds a Pointer

(defprotocol UserData
  (user-data ^org.bytedeco.javacpp.Pointer [this] "Returns the userdata pointer, or nil.")
  (set-user-data! [this p] "Sets the userdata pointer."))

(extend-protocol UserData
  liquidfun$b2Body
  (user-data [this] (when-let [p (.GetUserData this)] (when-not (.isNull p) p)))
  (set-user-data! [this p] (.SetUserData this p))
  liquidfun$b2Fixture
  (user-data [this] (when-let [p (.GetUserData this)] (when-not (.isNull p) p)))
  (set-user-data! [this p] (.SetUserData this p))
  liquidfun$b2Joint
  (user-data [this] (when-let [p (.GetUserData this)] (when-not (.isNull p) p)))
  (set-user-data! [this p] (.SetUserData this p))
  liquidfun$b2ParticleGroup
  (user-data [this] (when-let [p (.GetUserData this)] (when-not (.isNull p) p)))
  (set-user-data! [this p] (.SetUserData this p)))

;; ### Fixtures

(defn fixture-def
  "A FixtureDef defines  ashape with some physical properties.
  Do not call this directly, instead use `(body!)` or `(fixture!)`.

  Defaults, same as Box2D: density 0, friction 0.2, restitution 0.

  `:group-index` allows a certain group of objects to never
  collide (negative) or always collide (positive). Zero means no
  collision group.

  `:category-bits defaults to 0x0001, :mask-bits defaults to 0xFFFF`"
  [{:keys [shape density friction restitution is-sensor
           group-index category-bits mask-bits
           user-data]
    :or {density 0.0, friction 0.2, restitution 0.0, is-sensor false}}]
  (let [fd ($b2 FixtureDef.)]
    (doto fd
      (.shape shape)
      (.density density)
      (.friction friction)
      (.restitution restitution)
      (.isSensor is-sensor))
    (when (or group-index category-bits mask-bits)
      (let [ff (.filter fd)]
        (when group-index (.groupIndex ff group-index))
        (when category-bits (.categoryBits ff category-bits))
        (when mask-bits (.maskBits ff mask-bits))))
    (when user-data
      (.userData fd user-data))
    fd))

(defn fixture!
  "Creates a Fixture on an existing Body. The second argument is a
   fixture specification map to be passed to the `fixture-def`
   function."
  ^liquidfun$b2Fixture [^liquidfun$b2Body body fixture-spec]
  (.CreateFixture body (fixture-def fixture-spec)))

;; ### Bodies

(defn body-def
  "A BodyDef, which holds properties but not shapes. Do not call this
   directly, instead use `(body!)`."
  [{:keys [type position angle bullet fixed-rotation gravity-scale
           angular-damping linear-damping
           angular-velocity linear-velocity
           user-data]
    :or {type :dynamic, position [0.0 0.0], angle 0.0,
         bullet false, fixed-rotation false, gravity-scale 1.0,
         angular-damping 0.0, linear-damping 0.0,
         angular-velocity 0.0}}]
  (let [bd ($b2 BodyDef.)
        [x y] position]
    (doto bd
      (.type (body-types type))
      (.SetPosition x y)
      (.angle angle)
      (.bullet bullet)
      (.fixedRotation fixed-rotation)
      (.gravityScale gravity-scale)
      (.angularDamping angular-damping)
      (.linearDamping linear-damping)
      (.angularVelocity angular-velocity))
    (when-let [[x y] linear-velocity]
      (.Set (.linearVelocity bd) x y))
    (when user-data
      (.userData bd user-data))
    bd))

(defn body!
  "Creates a Body together with its Fixtures. The `body-spec` map is
   passed to the `body-def` function. Each of the `fixture-specs` are
   passed to the `fixture-def` function."
  ^liquidfun$b2Body [^liquidfun$b2World world body-spec & fixture-specs]
  (let [bd (body-def body-spec)
        bod (.CreateBody world bd)]
    (doseq [fspec fixture-specs]
      (fixture! bod fspec))
    bod))

;; ### Particle Systems

(def kw->particle-flag
  {:water liquidfun/b2_waterParticle ; Water particle.
   :zombie liquidfun/b2_zombieParticle ; Removed after next simulation step.
   :wall liquidfun/b2_wallParticle ; Zero velocity.
   :spring liquidfun/b2_springParticle ; With restitution from stretching.
   :elastic liquidfun/b2_elasticParticle ; With restitution from deformation.
   :viscous  liquidfun/b2_viscousParticle ; With viscosity.
   :powder liquidfun/b2_powderParticle ; Without isotropic pressure.
   :tensile liquidfun/b2_tensileParticle ; With surface tension.
   :color-mixing liquidfun/b2_colorMixingParticle ; Mix color between contacting particles.
   :destruction-listener liquidfun/b2_destructionListenerParticle ; Call b2DestructionListener on destruction.
   :barrier liquidfun/b2_barrierParticle ; Prevents other particles from leaking.
   :static-pressure liquidfun/b2_staticPressureParticle ; Less compressibility.
   :reactive liquidfun/b2_reactiveParticle ; Makes pairs or triads with other particles.
   :repulsive liquidfun/b2_repulsiveParticle ; With high repulsive force.
   ;; Call b2ContactListener when this particle is about to interact with
   ;; a rigid body or stops interacting with a rigid body.
   ;; This results in an expensive operation compared to using
   ;; b2_fixtureContactFilterParticle to detect collisions between
   ;; particles.
   :fixture-contact-listener liquidfun/b2_fixtureContactListenerParticle
   ;; Call b2ContactListener when this particle is about to interact with
   ;; another particle or stops interacting with another particle.
   ;; This results in an expensive operation compared to using
   ;; b2_particleContactFilterParticle to detect collisions between
   ;; particles.
   :particle-contact-listener liquidfun/b2_particleContactListenerParticle
   ;; Call b2ContactFilter when this particle interacts with rigid bodies.
   :fixture-contact-filter liquidfun/b2_fixtureContactFilterParticle
   ;; Call b2ContactFilter when this particle interacts with other
   ;; particles.
   :particle-contact-filter liquidfun/b2_particleContactFilterParticle})

(defn particle-flags
  [kw-set]
  (->> kw-set
       (map kw->particle-flag)
       (reduce bit-or)))

(def kw->particle-group-flag
  {:solid liquidfun/b2_solidParticleGroup
   :rigid liquidfun/b2_rigidParticleGroup
   :can-be-empty liquidfun/b2_particleGroupCanBeEmpty})

(defn particle-group-flags
  [kw-set]
  (->> kw-set
       (map kw->particle-group-flag)
       (reduce bit-or)))

(defn particle-color
  [[r g b a]]
  (-> ($b2 ParticleColor.)
      (.r r) (.g g) (.b b) (.a a)))

(defn particle-group-def
  [{:keys [angle angular-velocity color flags group group-flags
           lifetime linear-velocity particle-count position-data
           position shape shapes strength stride user-data]
    :or {angle 0
         angular-velocity 0
         lifetime 0.0
         position [0 0]
         strength 1
         stride 0}}]
  (let [pgd ($b2 ParticleGroupDef.)
        [x y] position]
    (when shape
      (.shape pgd shape))
    (when shapes
      ;; we can't allocate PointerPointer here as will be gc'd
      ;; so require that 'shapes' is passed in as a PointerPointer
      (.shapes pgd shapes))
    (when position-data
      (.positionData pgd position-data)
      (.particleCount pgd particle-count))
    (doto pgd
      (.SetPosition x y)
      (.angle angle)
      (.angularVelocity angular-velocity)
      (.lifetime lifetime)
      (.strength strength)
      (.stride stride))
    (when (seq flags)
      (.flags pgd (particle-flags flags)))
    (when (seq group-flags)
      (.groupFlags pgd (particle-group-flags group-flags)))
    (when-let [[x y] linear-velocity]
      (.Set (.linearVelocity pgd) x y))
    (when-let [[r g b a] color]
      (.SetColor pgd r g b a))
    (when group
      (.group pgd group))
    (when user-data
      (.userData pgd user-data))
    pgd))

(defn particle-group!
  ^liquidfun$b2ParticleGroup [^liquidfun$b2ParticleSystem ps pg-spec]
  (.CreateParticleGroup ps (particle-group-def pg-spec)))

(defn particle-system-def
  "Do not call this directly, instead use `(particle-system!)`."
  [{:keys [color-mixing-strength damping-strength density destroy-by-age
           ejection-strength elastic-strength gravity-scale lifetime-granularity
           max-count powder-strength pressure-strength radius
           repulsive-strength spring-strength
           static-pressure-iterations static-pressure-relaxation
           static-pressure-strength strict-contact-check
           surface-tension-normal-strength
           surface-tension-pressure-strength viscous-strength]
    :or {color-mixing-strength 0.5
         damping-strength 1.0
         density 1.0
         destroy-by-age true
         gravity-scale 1.0
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
  (let [psd ($b2 ParticleSystemDef.)]
    (when max-count
      (.maxCount psd max-count))
    (when strict-contact-check
      (.strictContactCheck psd strict-contact-check))
    (doto psd
      (.colorMixingStrength color-mixing-strength)
      (.dampingStrength damping-strength)
      (.density density)
      (.destroyByAge destroy-by-age)
      (.ejectionStrength ejection-strength)
      (.elasticStrength elastic-strength)
      (.gravityScale gravity-scale)
      (.lifetimeGranularity lifetime-granularity)
      (.powderStrength powder-strength)
      (.pressureStrength pressure-strength)
      (.radius radius)
      (.repulsiveStrength repulsive-strength)
      (.springStrength spring-strength)
      (.staticPressureIterations static-pressure-iterations)
      (.staticPressureRelaxation static-pressure-relaxation)
      (.staticPressureStrength static-pressure-strength)
      (.surfaceTensionNormalStrength surface-tension-normal-strength)
      (.surfaceTensionPressureStrength surface-tension-pressure-strength)
      (.viscousStrength viscous-strength))
    psd))

(defn particle-system!
  "Creates a Particle System with its groups. The `psd-spec` map is
   passed to the `particle-system-def` function. Each of the
   `group-specs` are passed to the `particle-group-def` function."
  ^liquidfun$b2ParticleSystem [^liquidfun$b2World world psd-spec & group-specs]
  (let [psd (particle-system-def psd-spec)
        ps (.CreateParticleSystem world psd)]
    (doseq [gspec group-specs]
      (particle-group! ps gspec))
    ps))

(defn particle-sys-seq
  "Lazy seq of all particle systems in the world."
  [^liquidfun$b2World world]
  (letfn [(nextstep [^liquidfun$b2ParticleSystem ps]
            (when ps (cons ps (nextstep (.GetNext ps)))))]
    (lazy-seq (nextstep (.GetParticleSystemList world)))))

(defn particle-group-seq
  "Lazy seq of all particle groups in the particle system."
  [^liquidfun$b2ParticleSystem ps]
  (letfn [(nextstep [^liquidfun$b2ParticleGroup pg]
            (when pg (cons pg (nextstep (.GetNext pg)))))]
    (lazy-seq (nextstep (.GetParticleGroupList ps)))))

(defn particle-positions
  [^liquidfun$b2ParticleSystem ps]
  (let [b (.GetPositionBuffer ps)
        n (.GetParticleCount ps)]
    ;; note there is also ^ByteBuffer (.asDirectBuffer b)
    (v2arr->seq b n)))

(defn stuck-candidates
  [^liquidfun$b2ParticleSystem ps]
  (let [n (.GetStuckCandidateCount ps)]
    (ptr->ints (.GetStuckCandidates ps) n)))

(defn destroy-particle!
  [^liquidfun$b2ParticleSystem ps i]
  (.DestroyParticle ps i))

(defn destroy-oldest-particle!
  [^liquidfun$b2ParticleSystem ps i call-destruction-listener?]
  (.DestroyOldestParticle ps i  call-destruction-listener?))

(defn particle-def
  ^liquidfun$b2ParticleDef
  [{:keys [color flags group lifetime velocity position user-data]
    :or {lifetime 0.0
         position [0 0]}}]
  (let [pd ($b2 ParticleDef.)
        [x y] position]
    (doto pd
      (.SetPosition x y)
      (.lifetime lifetime))
    (when (seq flags)
      (.flags pd (particle-flags flags)))
    (when-let [[x y] velocity]
      (.Set (.velocity pd) x y))
    (when-let [[r g b a] color]
      (.SetColor pd r g b a))
    (when group
      (.group pd group))
    (when user-data
      (.userData pd user-data))
    pd))

(defn particle!
  [^liquidfun$b2ParticleSystem ps particle-def]
  (.CreateParticle ps particle-def))

;; ## Query of objects

(defn body-of
  "Get the body to which a fixture belongs"
  ^liquidfun$b2Body [^liquidfun$b2Fixture fixt]
  (.GetBody fixt))

(defn bodyseq
  "Lazy seq of all bodies in the world."
  ([^liquidfun$b2World world]
   (bodyseq world (.GetBodyList world)))
  ([^liquidfun$b2World world ^liquidfun$b2Body body]
   (lazy-seq (when body (cons body (bodyseq world (.GetNext body)))))))

(defn fixtureseq
  "Lazy seq of fixtures on a body."
  [^liquidfun$b2Body body]
  (letfn [(nextstep [^liquidfun$b2Fixture fl]
            (when fl (cons fl (nextstep (.GetNext fl)))))]
    (lazy-seq (nextstep (.GetFixtureList body)))))

(defn fixture-of
  "Often a body will only have one fixture. This is a convenience
   function to pull out the first fixture from a body."
  [^liquidfun$b2Body body]
  (.GetFixtureList body))

;; ### Coordinate frames

(defprotocol EmbodiedRigid
  (to-local [this pt]
    "Local coordinates of a world point.")
  (position [this] [this local-pt]
    "World coordinates of a local point, default [0 0].")
  (center [this] "Center of mass in world coordinates.")
  (mass [this] "Total mass in kg.")
  (angle [this] "Angle in radians")
  (angular-velocity [this]
    "Angular velocity of a body in radians/second.")
  (linear-velocity [this] [this local-pt]
    "Linear velocity of a point on the body in local coordinates, by
     default its center of mass. In m/s.")
  (linear-velocity-world [this pt]
    "Linear velocity of a point on the body in world coordinates, m/s."))

(extend-protocol EmbodiedRigid

  liquidfun$b2Body
  (to-local [this pt]
    (v2xy (.GetLocalPoint this (vec2 pt))))
  (position
   ([this local-pt]
    (v2xy (.GetWorldPoint this (vec2 local-pt))))
   ([this]
    (v2xy (.GetPosition this))))
  (center [this]
    (v2xy (.GetWorldCenter this)))
  (mass [this]
    (.GetMass this))
  (angle [this]
    (.GetAngle this))
  (angular-velocity [this]
    (.GetAngularVelocity this))
  (linear-velocity
   ([this]
    (v2xy (.GetLinearVelocity this)))
   ([this local-pt]
    (v2xy (.GetLinearVelocityFromLocalPoint this (vec2 local-pt)))))
  (linear-velocity-world [this pt]
    (v2xy (.GetLinearVelocityFromWorldPoint this (vec2 pt))))

  liquidfun$b2ParticleGroup
  (to-local [this pt]
    (v2xy (liquidfun/b2MulT (.GetTransform this) (vec2 pt))))
  (position
   ([this local-pt]
    (v2xy (liquidfun/b2Mul (.GetTransform this) (vec2 local-pt))))
   ([this]
    (v2xy (.GetPosition this))))
  (center [this]
    (v2xy (.GetCenter this)))
  (mass [this]
    (.GetMass this))
  (angle [this]
    (.GetAngle this))
  (angular-velocity [this]
    (.GetAngularVelocity this))
  (linear-velocity
   ([this]
    (v2xy (.GetLinearVelocity this)))
   ([this local-pt]
    (linear-velocity-world this (position this local-pt))))
  (linear-velocity-world [this pt]
    (v2xy (.GetLinearVelocityFromWorldPoint this (vec2 pt)))))

(defn to-local-vect
  "Local vector of a world vector."
  [^liquidfun$b2Body this vect]
  (v2xy (.GetLocalVector this (vec2 vect))))

(defn fixture-mass
  "Fixture mass in kg."
  [^liquidfun$b2Fixture fixt]
  (let [md ($b2 MassData.)]
    (.GetMassData fixt md)
    (.mass md)))

(defn shape-loc-center
  "Shape center of mass in local coordinates."
  [^liquidfun$b2Shape shp]
  (if (instance? liquidfun$b2CircleShape shp)
    (v2xy (.m_p ^liquidfun$b2CircleShape shp))
    (let [md ($b2 MassData.)]
      (.ComputeMass shp md 0.0)
      (v2xy (.center md)))))

(defn fixture-loc-center
  [^liquidfun$b2Fixture fixt]
  (shape-loc-center (.GetShape fixt)))

(defn fixture-radius
  "Radius of a Fixture's shape."
  [^liquidfun$b2Fixture fixt]
  (.m_radius (.GetShape fixt)))

(defn local-coords
  "Local coordinates of polygon vertices. Approximated for circles."
  [^liquidfun$b2Fixture fixt]
  (case (shape-type fixt)
    :circle
    (let [this ($b2 CircleShape. (.GetShape fixt))
          r (.m_radius this)
          cent (shape-loc-center this)]
      (for [a (range (- PI) PI (/ TWOPI 30))]
        (v-add cent (polar-xy r a))))
    :polygon
    (let [this ($b2 PolygonShape. (.GetShape fixt))
          n (.GetVertexCount this)]
      (mapv (fn [i] (v2xy (.GetVertex this i))) (range n)))
    :edge
    (let [this ($b2 EdgeShape. (.GetShape fixt))]
      [(v2xy (.m_vertex1 this))
       (v2xy (.m_vertex2 this))])
    :chain
    (let [this ($b2 ChainShape. (.GetShape fixt))]
      (v2arr->seq (.m_vertices this) (.m_count this)))))

(defn world-coords
  "World coordinates of polygon vertices. Approximated for circles."
  [^liquidfun$b2Fixture fixt]
  (let [body (.GetBody fixt)]
    (mapv #(position body %) (local-coords fixt))))

;; ## Movement

(defn apply-force!
  "Apply a force in Newtons to body at a world point. If the force is
   not applied at the center of mass, it will generate a torque and
   affect the angular velocity. This wakes up the body."
  [^liquidfun$b2Body body force pt]
  (.ApplyForce body (vec2 force) (vec2 pt) true))

(defn apply-torque!
  "Apply a torque in N-m, i.e. about the z-axis (out of the screen).
   This affects the angular velocity without affecting the linear
   velocity of the center of mass. This wakes up the body."
  [^liquidfun$b2Body body torque]
  (.ApplyTorque body torque true))

(defn apply-impulse!
  "Apply an impulse in N-seconds or kg-m/s at a point. This
   immediately modifies the velocity. It also modifies the angular
   velocity if the point of application is not at the center of mass.
   This wakes up the body."
  [^liquidfun$b2Body body impulse pt]
  (.ApplyLinearImpulse body (vec2 impulse) (vec2 pt) true))

(defn linear-velocity!
  [^liquidfun$b2Body body vel]
  ;(.SetLinearVelocity body (vec2 vel))
  (let [[x y] vel]
    (.Set (.GetLinearVelocity body) x y)))

(defn angular-velocity!
  [^liquidfun$b2Body body a-vel]
  (.SetAngularVelocity body a-vel))

(defn awake?
  [^liquidfun$b2Body body]
  (.IsAwake body))

(defn wake!
  "Wake up a body."
  [^liquidfun$b2Body body]
  (.SetAwake body true))

(defn sleep!
  "Put a body to sleep."
  [^liquidfun$b2Body body]
  (.SetAwake body false))

(defn destroy-body!
  "Remove object from the World permanantly. Destroying a body
  automatically deletes all associated shapes and joints."
  [^liquidfun$b2Body this]
  (.DestroyBody (.GetWorld this) this))

(defn destroy-joint!
  [^liquidfun$b2Joint this]
  (.DestroyJoint (.GetWorld (.GetBodyA this)) this))

(defn destroy-fixture!
  [^liquidfun$b2Fixture this]
  (.DestroyFixture (.GetBody this) this))

(defn destroy-particle-system!
  [^liquidfun$b2World world this]
  (.DestroyParticleSystem world this))

;; ### Spatial queries

(defn aabb
  "Axis-Aligned Bounding Box"
  ([[x0 y0] [x1 y1]]
   (let [a ($b2 AABB.)]
     (.Set (.lowerBound a) (min x0 x1) (min y0 y1))
     (.Set (.upperBound a) (max x0 x1) (max y0 y1))
     a))
  ([^liquidfun$b2Fixture fixt]
   (let [aabb* ($b2 AABB.)]
     (.ComputeAABB (.GetShape fixt) aabb*
                   (.GetTransform (.GetBody fixt))
                   0)
     aabb*)))

(defn query-aabb
  "Return a vector of (up to a given number of) fixtures overlapping
   an Axis-Aligned Bounding Box"
  ([world bb]
   (query-aabb world bb 1000000))
  ([^liquidfun$b2World world ^liquidfun$b2AABB bb max-take]
   (let [fxx (atom [])
         cb (proxy [liquidfun$b2QueryCallback] []
              (ReportFixture [fixt]
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
  ([^liquidfun$b2World world [x y] max-take]
   (let [bb (aabb [(- x 0.001) (- y 0.001)]
                  [(+ x 0.001) (+ y 0.001)])
         fxx (atom [])
         pt-vec2 (vec2 [x y])
         cb (proxy [liquidfun$b2QueryCallback] []
              (ReportFixture [^liquidfun$b2Fixture fixt]
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
  [^liquidfun$b2World world start-pt end-pt mode & {:keys [ignore]}]
  (let [fxx (atom ())
        cb (proxy [liquidfun$b2RayCastCallback] []
             (ReportFixture [fixt pt norm frac]
               (if (and ignore (ignore fixt))
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

(defn contact-data
  "Returns a map with keys :fixture-a :fixture-b :points :normal
   :normal-impulses :tangent-impulses from a JBox2D Contact object and
   optional ContactImpulse object. Returns nil if no contact points
   exist."
  ([contact]
   (contact-data contact nil))
  ([^liquidfun$b2Contact contact impulses]
   (when (.IsTouching contact)
     (contact-data contact impulses ($b2 WorldManifold.) false)))
  ([^liquidfun$b2Contact contact ^liquidfun$b2ContactImpulse impulses
    ;; just to reuse memory:
    ^liquidfun$b2WorldManifold world-manifold check-touching?]
   (when (or (not check-touching?) (.IsTouching contact))
     (let [manifold (.GetManifold contact)
           pcount (.pointCount manifold)]
       (when (pos? pcount)
         ;; mutates its argument:
         (.GetWorldManifold contact world-manifold)
         (let [fixt-a (.GetFixtureA contact)
               fixt-b (.GetFixtureB contact)
               pts (v2arr->seq (.points world-manifold) pcount)
               normal (v2xy (.normal world-manifold))]
           (ContactData. fixt-a fixt-b pts normal
                         (when impulses
                           (ptr->floats (.normalImpulses impulses) pcount))
                         (when impulses
                           (ptr->floats (.tangentImpulses impulses) pcount)))))))))

(defn buffering-contact-listener
  "Returns a ContactListener which stores contacts, as a tuple [listener atom].
  The atom will be populated with a sequence of `contact-data` records. Consumer
  is responsible for emptying it."
  []
  (let [a (atom ())
        wm ($b2 WorldManifold.)
        lstnr (proxy [liquidfun$b2ContactListener] []
                (PostSolve [contact impulses]
                  (if-let [cd (contact-data contact impulses wm true)]
                    (swap! a conj cd))))]
    [lstnr a]))

(defn current-contacts
  "Lazy seq of contacts on this body. Each contact is a map as defined
   by the `contact-data` function. Contacts without actual contact
   points (i.e. created only due to overlapping bounding boxes) are
   excluded."
  [^liquidfun$b2Body bod]
  (letfn [(nextstep [^liquidfun$b2ContactEdge cl]
            (when cl
              (if-let [cdata (contact-data (.contact cl))]
                (cons cdata (nextstep (.next cl)))
                (nextstep (.next cl)))))]
    (lazy-seq (nextstep (.GetContactList bod)))))

(defn contacting
  "Set of other bodies that the given body is currently contacting."
  [^liquidfun$b2Body bod]
  (let [bodies (mapcat #(list (body-of (:fixture-a %))
                              (body-of (:fixture-b %)))
                       (current-contacts bod))]
    (set (remove #(= bod %) bodies))))

;; # Joints

;; ## Creation of joints

(defmulti joint-def*
  :type)

(defn joint-def
  [{:keys [user-data collide-connected] :as spec}]
  (let [jd ^liquidfun$b2JointDef (joint-def* spec)]
    (when collide-connected
      (.collideConnected jd collide-connected))
    (when user-data
      (.userData jd user-data))
    jd))

(defmulti init-joint-spec
  "Augments the joint specification map to define body-local anchors,
   axes, etc. from given initial world values (e.g. `:world-anchor`)."
  :type)

(defn joint-from-def!
  [^liquidfun$b2JointDef joint-def]
  (.CreateJoint (.GetWorld (.bodyA joint-def)) joint-def))

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
  ^liquidfun$b2Joint [spec]
  (let [jt (joint-from-def! (joint-def (init-joint-spec spec)))]
    ;; pointer cast so that class-based dispatch works
    (classy-joint jt)))

(defmethod init-joint-spec :default
  [spec]
  spec)

(defmethod init-joint-spec :revolute
  [{:keys [body-a body-b world-anchor]
    :as spec}]
  (if (:anchor-a spec) ;; prefer local spec
    spec
    (assoc spec
      :anchor-a (to-local body-a world-anchor)
      :anchor-b (to-local body-b world-anchor)
      :reference-angle (- (angle body-b) (angle body-a)))))

(defmethod joint-def* :revolute
  [{:keys [body-a body-b anchor-a anchor-b reference-angle
           enable-motor motor-speed max-motor-torque
           enable-limit lower-angle upper-angle]
    :or {reference-angle 0
         enable-motor false, motor-speed 0, max-motor-torque 10000,
         enable-limit false, lower-angle 0, upper-angle TWOPI}}]
  (let [jd ($b2 RevoluteJointDef.)]
    (let [[x y] anchor-a] (.Set (.localAnchorA jd) x y))
    (let [[x y] anchor-b] (.Set (.localAnchorB jd) x y))
    (doto jd
      (.bodyA body-a)
      (.bodyB body-b)
      (.referenceAngle reference-angle)
      (.enableMotor enable-motor)
      (.motorSpeed motor-speed)
      (.maxMotorTorque max-motor-torque)
      (.enableLimit enable-limit)
      (.lowerAngle lower-angle)
      (.upperAngle upper-angle))
    jd))

(defmethod init-joint-spec :prismatic
  [{:keys [body-a body-b world-anchor world-axis]
    :as spec}]
  (if (:anchor-a spec) ;; prefer local spec
    spec
    (assoc spec
      :anchor-a (to-local body-a world-anchor)
      :anchor-b (to-local body-b world-anchor)
      :axis-a (to-local-vect body-a world-axis)
      :reference-angle (- (angle body-b) (angle body-a)))))

(defmethod joint-def* :prismatic
  [{:keys [body-a body-b anchor-a anchor-b axis-a reference-angle
           enable-motor motor-speed max-motor-force
           enable-limit lower-trans upper-trans]
    :or {reference-angle 0
         enable-motor false, motor-speed 0, max-motor-force 10000,
         enable-limit false, lower-trans -10, upper-trans 10}}]
  (let [jd ($b2 PrismaticJointDef.)]
    (let [[x y] anchor-a] (.Set (.localAnchorA jd) x y))
    (let [[x y] anchor-b] (.Set (.localAnchorB jd) x y))
    (let [[x y] axis-a] (.Set (.localAxisA jd) x y))
    (doto jd
      (.bodyA body-a)
      (.bodyB body-b)
      (.referenceAngle reference-angle)
      (.enableMotor enable-motor)
      (.motorSpeed motor-speed)
      (.maxMotorForce max-motor-force)
      (.enableLimit enable-limit)
      (.lowerTranslation lower-trans)
      (.upperTranslation upper-trans))
    jd))

(defmethod init-joint-spec :distance
  [{:keys [body-a body-b world-anchor-a world-anchor-b]
    :as spec}]
  (if (:anchor-a spec) ;; prefer local spec
    spec
    (assoc spec
      :anchor-a (to-local body-a world-anchor-a)
      :anchor-b (to-local body-b world-anchor-b)
      :length (v-dist world-anchor-a world-anchor-b))))

(defmethod joint-def* :distance
  [{:keys [body-a anchor-a body-b anchor-b length
           frequency-hz damping-ratio]
    :or {length 1.0, frequency-hz 0, damping-ratio 0}}]
  (let [jd ($b2 DistanceJointDef.)]
    (let [[x y] anchor-a] (.Set (.localAnchorA jd) x y))
    (let [[x y] anchor-b] (.Set (.localAnchorB jd) x y))
    (doto jd
      (.bodyA body-a)
      (.bodyB body-b)
      (.length length)
      (.frequencyHz frequency-hz)
      (.dampingRatio damping-ratio))
    jd))

(defmethod joint-def* :rope
  [{:keys [body-a anchor-a body-b anchor-b max-length]
    :or {max-length 1.0}}]
  (let [jd ($b2 RopeJointDef.)]
    (let [[x y] anchor-a] (.Set (.localAnchorA jd) x y))
    (let [[x y] anchor-b] (.Set (.localAnchorB jd) x y))
    (doto jd
      (.bodyA body-a)
      (.bodyB body-b)
      (.maxLength max-length))
    jd))

(defmethod joint-def* :mouse
  [{:keys [body-a body-b target max-force
           frequency-hz damping-ratio]
    :or {max-force 1000,
         frequency-hz 5, damping-ratio 0.7}}]
  (let [jd ($b2 MouseJointDef.)]
    (let [[x y] target] (.Set (.target jd) x y))
    (doto jd
      (.bodyA body-a)
      (.bodyB body-b)
      (.maxForce max-force)
      (.frequencyHz frequency-hz)
      (.dampingRatio damping-ratio))
    jd))

(defmethod joint-def* :gear
  [{:keys [body-a body-b joint-1 joint-2 ratio]}]
  (let [jd ($b2 GearJointDef.)]
    (doto jd
      (.bodyA body-a)
      (.bodyB body-b)
      (.joint1 joint-1)
      (.joint2 joint-2)
      (.ratio ratio))
    jd))

(defmethod init-joint-spec :weld
  [{:keys [body-a body-b world-anchor]
    :as spec}]
  (if (:anchor-a spec) ;; prefer local spec
    spec
    (assoc spec
      :anchor-a (to-local body-a world-anchor)
      :anchor-b (to-local body-b world-anchor)
      :reference-angle (- (angle body-b) (angle body-a)))))

(defmethod joint-def* :weld
  [{:keys [body-a body-b anchor-a anchor-b reference-angle]
    :or {reference-angle 0}}]
  (let [jd ($b2 WeldJointDef.)]
    (let [[x y] anchor-a] (.Set (.localAnchorA jd) x y))
    (let [[x y] anchor-b] (.Set (.localAnchorB jd) x y))
    (doto jd
      (.bodyA body-a)
      (.bodyB body-b)
      (.referenceAngle reference-angle))
    jd))

;; ## Query of joints

(defn- jointseq*
  "Lazy seq of joints in a joint list"
  [^liquidfun$b2Joint joint]
  (lazy-seq (when joint (cons (classy-joint joint)
                              (jointseq* (.GetNext joint))))))

(defn jointseq
  "Lazy seq of all joints connected to a body"
  [^liquidfun$b2Body body]
  (jointseq* (.GetJointList body)))

(defn alljointseq
  "Lazy seq of all joints in the world."
  [^liquidfun$b2World world]
  (jointseq* (.GetJointList world)))

(defn body-a
  "Return bodyA for a joint"
  [^liquidfun$b2Joint jt]
  (.GetBodyA jt))

(defn body-b
  "Return bodyB for a joint"
  [^liquidfun$b2Joint jt]
  (.GetBodyB jt))

(defn anchor-a
  "The anchor point on bodyA in world coordinates"
  [^liquidfun$b2Joint jt]
  (v2xy (.GetAnchorA jt)))

(defn anchor-b
  "The anchor point on bodyB in world coordinates"
  [^liquidfun$b2Joint jt]
  (v2xy (.GetAnchorB jt)))

(defn reaction-force
  "The reaction force on bodyB at the joint anchor in Newtons.
   Give the inverse of the timestep."
  [^liquidfun$b2Joint jt inv-dt]
  (v2xy (.GetReactionForce jt inv-dt)))

(defn reaction-torque
  "Get the reaction torque due to the joint limit given the inverse time step. Unit is N*m."
  [^liquidfun$b2Joint jt inv-dt]
  (.GetReactionTorque jt inv-dt))

(defn joint-angular-velocity
  "Relative angular velocity of two attached bodies in radians/second."
  [jt]
  (- (angular-velocity (body-b jt))
     (angular-velocity (body-a jt))))

(defn joint-angle
  [jt]
  (let [jt ($b2 RevoluteJoint. jt)]
    (in-pi-pi (.GetJointAngle jt))))

(defn joint-translation
  [jt]
  (let [jt ($b2 PrismaticJoint. jt)]
    (.GetJointTranslation jt)))

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

  liquidfun$b2RevoluteJoint
  (limit-enabled? [this] (.IsLimitEnabled this))
  (enable-limit! [this flag] (.EnableLimit this flag))
  (limits [this] [(.GetLowerLimit this) (.GetUpperLimit this)])
  (limits! [this [a b]] (.SetLimits this a b))

  liquidfun$b2PrismaticJoint
  (limit-enabled? [this] (.IsLimitEnabled this))
  (enable-limit! [this flag] (.EnableLimit this flag))
  (limits [this] [(.GetLowerLimit this) (.GetUpperLimit this)])
  (limits! [this [a b]] (.SetLimits this a b)))

;; ## Motors

;; There's no interface so let's impose a protocol.
;; One could call the java methods directly but these avoid reflection.

(defprotocol Motorised
  "Abstraction for JBox2D joints which can have motors"
  (motor-enabled? [this] "Whether a motor is enabled on a joint.")
  (enable-motor! [this flag] "Set whether a motor is enabled.")
  (motor-speed [this] "Motor (target) speed, may or may not be enabled.")
  (motor-speed! [this speed] "Set motor (target) speed, may or may not be enabled.")
  (motor-force [this inv-dt] "Current motor force given the inverse time step, usually in N.")
  (motor-torque [this inv-dt] "Current motor torque given the inverse time step, in N*m.")
  (max-motor-force [this] "Maximum motor force")
  (max-motor-torque [this] "Maximum motor torque")
  (max-motor-force! [this force] "Set maximum motor force")
  (max-motor-torque! [this torque] "Set maximum motor torque")
  (power-watts [this]
   "Instantaneous rate of work done by a motor in Watts.
   Multiply by the time step to get work in Joules."))

(extend-protocol Motorised

  liquidfun$b2RevoluteJoint
  (motor-enabled? [this] (.IsMotorEnabled this))
  (enable-motor! [this flag] (.EnableMotor this flag))
  (motor-speed [this] (.GetMotorSpeed this))
  (motor-speed! [this speed] (.SetMotorSpeed this speed))
  (motor-torque [this inv-dt] (.GetMotorTorque this inv-dt))
  (max-motor-torque [this] (.GetMaxMotorTorque this))
  (max-motor-torque! [this torque] (.SetMaxMotorTorque this torque))
  (power-watts [this] (* (.GetJointSpeed this) (motor-torque this 1.0))) ;; ??

  liquidfun$b2PrismaticJoint
  (motor-enabled? [this] (.IsMotorEnabled this))
  (enable-motor! [this flag] (.EnableMotor this flag))
  (motor-speed [this] (.GetMotorSpeed this))
  (motor-speed! [this speed] (.SetMotorSpeed this speed))
  (motor-force [this inv-dt] (.GetMotorForce this inv-dt))
  (max-motor-force [this] (.GetMaxMotorForce this))
  (max-motor-force! [this force] (.SetMaxMotorForce this force))
  (power-watts [this] (* (.GetJointSpeed this) (motor-force this 1.0))))

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
        :center (fixture-loc-center fixt))
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
