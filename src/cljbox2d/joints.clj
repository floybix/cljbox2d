(ns cljbox2d.joints
  (:use cljbox2d.core)
  (:import (org.jbox2d.dynamics Body World)
           (org.jbox2d.dynamics.joints JointType
                                       DistanceJoint DistanceJointDef
                                       RevoluteJoint RevoluteJointDef
                                       ConstantVolumeJoint ConstantVolumeJointDef)))

;; ENUMS

(def joint-types
  {:distance JointType/DISTANCE
   :revolute JointType/REVOLUTE
   :constant-volume JointType/CONSTANT_VOLUME})

(def joint-keywords
  (zipmap (vals joint-types) (keys joint-types)))

(defn joint-type
  [joint]
  (joint-keywords (.getType joint)))

;; JOINTS

(defn revolute-joint-def
  "A revolute joint constrains two bodies to share a common point
while they are free to rotate about the point. The relative rotation
about the shared point is the joint angle. You can limit the relative
rotation with a joint limit that specifies a lower and upper
angle. You can use a motor to drive the relative rotation about the
shared point. A maximum motor torque is provided so that infinite
forces are not generated."
  [body1 body2 anchor
   & {:keys [enable-motor motor-speed max-torque
             enable-limit lower-angle upper-angle
             collide-connected user-data]
      :or {enable-motor false motor-speed 0 max-torque 10000
           enable-limit false lower-angle 0 upper-angle 360
           collide-connected false}}]
  (let [jd (RevoluteJointDef.)]
    (.initialize jd body1 body2 (vec2 anchor))
    (set! (.enableMotor jd) enable-motor)
    (set! (.motorSpeed jd) motor-speed)
    (set! (.maxMotorTorque jd) max-torque)
    (set! (.enableLimit jd) enable-limit)
    (set! (.lowerAngle jd) lower-angle)
    (set! (.upperAngle jd) upper-angle)
    (set! (.collideConnected jd) collide-connected)
    (set! (.userData jd) user-data)
    jd))

(defn distance-joint-def
  "Distance joint definition. This requires defining an anchor point
on both bodies and the non-zero length of the distance joint. The
definition uses local anchor points so that the initial configuration
can violate the constraint slightly. This helps when saving and
loading a game.
Note however that this initialisation uses world points.
For :damping-ratio 0 = no damping; 1 = critical damping."
  [body1 body2 anchor1 anchor2
   & {:keys [frequency-hz damping-ratio
             collide-connected user-data]
      :or {frequency-hz 0 damping-ratio 0
           collide-connected false}}]
  (let [jd (DistanceJointDef.)]
    (.initialize jd body1 body2 (vec2 anchor1) (vec2 anchor2))
    (set! (.frequencyHz jd) frequency-hz)
    (set! (.dampingRatio jd) damping-ratio)
    (set! (.collideConnected jd) collide-connected)
    (set! (.userData jd) user-data)
    jd))

(defn joint!
  "Creates a Joint from a JointDef."
  [jd]
  (.createJoint *world* jd))

;; QUERY OF OBJECTS

(defn jointseq
  "Seq of all bodies in the world, or a joint list"
  ([]
     (jointseq (.getJointList *world*)))
  ([joint]
     (lazy-seq (if joint (cons joint (jointseq (.getNext joint)))))))

(defn anchor-a
  "The anchor point on bodyA in world coordinates"
  [jt]
  ;; yuck, the method modifies its argument
  (let [v0 (vec2 [0 0])]
    (.getAnchorA jt v0)
    (xy v0)))

(defn anchor-b
  "The anchor point on bodyB in world coordinates"
  [jt]
  ;; yuck, the method modifies its argument
  (let [v0 (vec2 [0 0])]
    (.getAnchorB jt v0)
    (xy v0)))
