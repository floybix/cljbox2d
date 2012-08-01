(ns cljbox2d.testbed
  (:use cljbox2d.core)
  (:require [quil.core :as quil])
  (:import (org.jbox2d.common Vec2)
           (org.jbox2d.dynamics Body BodyDef BodyType Fixture FixtureDef World)
           (org.jbox2d.dynamics.joints DistanceJoint RevoluteJoint)
           (org.jbox2d.collision.shapes PolygonShape CircleShape ShapeType MassData)))

; (load-file "/home/felix/devel/cljbox2d/src/cljbox2d/testbed.clj")

;; for now, just one specific test

;;; adapted from org.jbox2d.testbed.tests.VaryingRestitution

(def things (atom {}))

(defn setup-world! []
  (create-world!)
  (let [ground (body! (body-def :type :static)
                      (fixture-def (edge [-40 0] [40 0])))
        restns [0.0 0.1 0.3 0.5 0.75 0.9 1.0]
        balls (doall (for [[i r] (map-indexed list restns)]
                       (body! (body-def :position [(+ 10 (* 3 i)) 20])
                              (fixture-def (circle 1) :restitution r))))]
    {:ground ground :balls balls}))

(defn draw-world
  "Draw all shapes (fixtures) from the Box2D world"
  []
  (doseq [fx (fixtureseq)
          :let [pts (world-coords fx)
                pt0 (first pts)
                typ (shape-type fx)]]
    (case typ
      :circle (quil/ellipse (* 10 (pt0 0))
                            (- (quil/height) (* 10 (pt0 1)))
                            (* 10 (* 2 (radius fx)))
                            (* 10 (* 2 (radius fx))))
      :polygon (do
                 (quil/begin-shape) 
                 (doall (map #(apply quil/vertex %) pts))
                 (quil/end-shape)))))

(defn setup []
  (quil/frame-rate 30)
  (swap! things (fn [_] (setup-world!)))
)

(defn draw []
  (step! (/ 1 30))
  (quil/stroke 0)
  (quil/stroke-weight 1)
  (quil/fill 64)
  (quil/background 200)
  (draw-world))

(quil/defsketch test-sketch
  :title "cljbox2d test"
  :setup setup
  :draw draw
  :size [800 600])
