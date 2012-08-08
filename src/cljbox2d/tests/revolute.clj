(ns cljbox2d.tests.revolute
  (:use [cljbox2d core joints testbed])
  (:require [quil.core :as quil]))

;;; A translation of
;;; org.jbox2d.testbed.tests.RevoluteTest

(def things)

(def is-left (atom false))

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

(defn update-info-text []
  (let [jt (:joint things)]
    (reset! info-text
            (str "Limits " (if (.isLimitEnabled jt) "on" "off")
                 ", Motor " (if (.isMotorEnabled jt) "on " "off ")
                 (if @is-left "left" "right") "\n"
                 "Keys: (l) limits, (m) motor, (a) left, (d) right"))))

(defn key-press []
  (let [jt (:joint things)]
    (case (quil/raw-key)
      \l (.enableLimit jt (not (.isLimitEnabled jt)))
      \m (.enableMotor jt (not (.isMotorEnabled jt)))
      \a (do (.setMotorSpeed jt PI) (reset! is-left true))
      \d (do (.setMotorSpeed jt (- PI)) (reset! is-left false))
      :otherwise-ignore-it))
  (update-info-text))

(defn setup []
  (setup-style)
  (setup-world!)
  (update-info-text))

(defn draw []
  (step! (/ 1 (quil/current-frame-rate)))
  (quil/background 0)
  (draw-world))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Revolute"
    :setup setup
    :draw draw
    :key-typed key-press
    :mouse-dragged mouse-dragged
    :size [600 500]))
