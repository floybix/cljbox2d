(ns cljbox2d.core
  (:import (org.jbox2d.common Vec2)
           (org.jbox2d.dynamics Body BodyDef BodyType Fixture FixtureDef World)
           (org.jbox2d.dynamics.joints DistanceJoint RevoluteJoint)
           (org.jbox2d.collision.shapes PolygonShape CircleShape ShapeType MassData)))

;(set! *warn-on-reflection* true)

;; world

(def ^:dynamic *world*)

(defn create-world!
  ([]
     (create-world! [0 -10]))
  ([gravity]
     (alter-var-root *world* (fn [] (World. (vec2 gravity) true)))))
  
(defn vec2
  "Make a org.jbox2d.common.Vec2 from a clojure vector or two numbers.
  If v is already a Vec2, return it."
  ([v] (if (isa? (class v) Vec2)
         v
         (Vec2. (v 0) (v 1))))
  ([x y] (Vec2. x y)))

;; shapes

(defn circle
  "Create a circle shape"
  [radius]
  (let [shape (CircleShape.)]
    (set! (. shape m_radius) radius)
    shape))

(defn edge
  "Create an edge shape"
  [pt1 pt2]
  (let [shape (PolygonShape.)]
    (.setAsEdge shape (vec2 pt1) (vec2 pt2))
    shape))

(defn box
  "Create a box shape"
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
  "Create a polygon shape. Must be convex.
   It is assumed that the exterior is the right of each edge.
   i.e. vertices go counter-clockwise."
  [vertices]
  (let [shape (PolygonShape.)
        vv (to-array (map vec2 vertices))]
    (.set shape vv (count vertices))
    shape))

;; fixtures

(defn OLD-fixture-def
  "Create a Fixture definition, which is a shape with some physical properties:
   :density :friction :restitution :user-data"
  [shape opts]
  (let [fd (FixtureDef.)]
    (set! (.shape fd) shape)
    (set! (.density fd)  (:density opts 1.0))
    (set! (.friction fd) (:friction opts 0.3))
    (set! (.restitution fd) (:restitution opts 0.3))
    (set! (.userData fd) (:user-data opts))
    fd))

(defn fixture-def
  "Create a Fixture definition, which is a shape with some physical properties"
  [shape & {:keys [density friction restitution user-data]
            :or {density 1, friction 0.3, restitution 0.3}}]
  (let [fd (FixtureDef.)]
    (set! (.shape fd) shape)
    (set! (.density fd) density)
    (set! (.friction fd) friction)
    (set! (.restitution fd) restitution)
    (set! (.userData fd) user-data)
    fd))

(defn fixture-from-def
  "Creates a Fixture on an existing Body from a FixtureDef."
  [body fd]
  (let [fx (Fixture.)]
    (.create fx body fd)
    fx))

(defn fixture
  "Creates a Fixture on an existing Body.
   A convenience wrapper for (fixture-from-def body (fixture-def ...))"
  [body shape & opts]
  (fixture-from-def body (apply fixture-def shape opts)))

;; bodies

(def body-types
  {::dynamic BodyType/DYNAMIC
   ::static BodyType/STATIC
   ::kinematic BodyType/KINEMATIC})

(defn body-def
  "Creates a Body definition, which holds properties but not shapes."
  [& {:keys [type position angle bullet fixed-rotation
             angular-damping linear-damping user-data]
      :or {type ::dynamic, position [0 0], angle 0,
           bullet false, fixed-rotation false,
           angular-damping 0, linear-damping 0}}]
  (let [bd (BodyDef.)]
    (set! (.type bd) (body-types type))
    (set! (.position bd) (vec2 position))
    (set! (.angle bd) angle)
    (set! (.bullet bd) bullet)
    (set! (.fixedRotation bd) fixed-rotation)
    (set! (.angularDamping bd) angular-damping)
    (set! (.linearDamping bd) linear-damping)
    (set! (.userData bd) user-data)
    bd))

(defn body-from-def
  "Creates a Body from a BodyDef and optional FixtureDefs."
  [bd & fixture-defs]
  (let [bod (.createBody *world* bd)]
    (doseq [fd fixture-defs]
      (fixture-from-def bod fd))
    bod))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
