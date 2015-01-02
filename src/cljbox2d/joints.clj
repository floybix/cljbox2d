(ns cljbox2d.joints
  "Core API for joints."
  (:require [cljbox2d.core :refer :all]
            [cljbox2d.vec2d :refer [in-pi-pi v-dist]])
  (:import (org.jbox2d.dynamics World Body)
           (org.jbox2d.dynamics.joints Joint JointType
                                       ConstantVolumeJoint ConstantVolumeJointDef
                                       DistanceJoint DistanceJointDef
                                       RopeJoint RopeJointDef
                                       MouseJoint MouseJointDef
                                       PrismaticJoint PrismaticJointDef
                                       RevoluteJoint RevoluteJointDef
                                       WeldJoint WeldJointDef)))

;; ## Enums

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
         collide-connected false}}]
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
