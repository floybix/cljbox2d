(ns cljbox2d.tests.revolute
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.RevoluteTest"
  (:use [cljbox2d core joints testbed])
  (:require [quil.core :as quil]))

(def things (atom {}))

(def is-left (atom false))

(defn setup-world! []
  (create-world!)
  (let [ground (body! {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        w 100
        ball (body! {:position [0 20]
                     :angular-velocity w
                     :linear-velocity [(* -8 w) 0]}
                    {:shape (circle 0.5) :density 5})
        joint (revolute-joint! ground ball [0 12]
                               {:motor-speed (- PI)
                                :motor-torque 10000
                                :lower-angle (/ (- PI) 4)
                                :upper-angle (/ PI 2)
                                :enable-limit true
                                :collide-connected true})]
    (reset! things {:ball ball :joint joint})
    (reset! ground-body ground)))

(defn update-info-text []
  (let [jt (:joint @things)]
    (reset! info-text
            (str "Limits " (if (.isLimitEnabled jt) "on" "off")
                 ", Motor " (if (.isMotorEnabled jt) "on " "off ")
                 (if @is-left "left" "right") "\n"
                 "Keys: (l) limits, (m) motor, (a) left, (d) right"))))

(defn my-key-press []
  (let [jt (:joint @things)]
    (case (quil/raw-key)
      \l (.enableLimit jt (not (.isLimitEnabled jt)))
      \m (.enableMotor jt (not (.isMotorEnabled jt)))
      \a (do (.setMotorSpeed jt PI) (reset! is-left true))
      \d (do (.setMotorSpeed jt (- PI)) (reset! is-left false))
      ;; otherwise pass on to testbed
      (key-press)))
  (update-info-text))

(defn setup []
  (setup-world!)
  (update-info-text))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Revolute"
    :setup setup
    :draw draw
    :key-typed my-key-press
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :size [600 500]))
