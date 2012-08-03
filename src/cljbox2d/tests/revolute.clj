(ns cljbox2d.tests.revolute
  (:use [cljbox2d core joints testbed])
  (:require [quil.core :as quil]))

;;; A translation of
;;; org.jbox2d.testbed.tests.RevoluteTest

(def things)

(defn setup-world! []
  (create-world!)
  (let [ground (body! (body-def :type :static)
                      (fixture-def (edge [-40 0] [40 0])))
        w 100
        body (body! (body-def :position [0 20]
                              :angular-velocity w
                              :linear-velocity [(* -8 w) 0])
                    (fixture-def (circle 0.5) :density 5))
        joint (joint! (revolute-joint-def ground body [0 12]
                                          :motor-speed (- PI)
                                          :motor-torque 10000
                                          :lower-angle (/ (- PI) 4)
                                          :upper-angle (/ PI 2)
                                          :enable-limit true
                                          :collide-connected true))]
    (def things {:ground ground :ball body :joint joint})))

(defn key-press []
  (let [jt (:joint things)]
    (case (quil/raw-key)
      \l (.enableLimit jt (not (.isLimitEnabled jt)))
      \m (.enableMotor jt (not (.isMotorEnabled jt)))
      \a (.setMotorSpeed jt PI)
      \d (.setMotorSpeed jt (- PI))
      :otherwise-ignore-it)))

(defn setup []
  (setup-style)
  (setup-world!))

(defn draw []
  (step! (/ 1 (quil/current-frame-rate)))
  (quil/background 0)
  (draw-world))

(quil/defsketch test-sketch
  :title "Revolute"
  :setup setup
  :draw draw
  :key-typed key-press
  :size [600 500])
