(ns org.nfrac.cljbox2d.testbed.tests.revolute
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.RevoluteTest"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [org.nfrac.cljbox2d.core :refer :all]
            [quil.core :as quil]
            [quil.middleware]))

(def PI Math/PI)

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        w 100
        ball (body! world {:position [0 20]
                           :angular-velocity w
                           :linear-velocity [(* -8 w) 0]}
                    {:shape (circle 0.5) :density 5})
        joint (joint! {:type :revolute
                       :body-a ground
                       :body-b ball
                       :world-anchor [0 12]
                       :motor-speed (- PI)
                       :max-motor-torque 10000
                       :lower-angle (/ (- PI) 4)
                       :upper-angle (/ PI 2)
                       :enable-limit true
                       :collide-connected true})]
    (assoc bed/initial-state
      :world world
      ::things {:ball ball :joint joint})))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn draw
  [state]
  (bed/draw state)
  (let [jt (:joint (::things state))]
    (quil/fill 255)
    (quil/text (str "Limits " (if (limit-enabled? jt) "on" "off")
                    ", Motor " (if (motor-enabled? jt) "on " "off ")
                    (if (pos? (motor-speed jt)) "left" "right") "\n"
                    "Keys: (l) limits, (m) motor, (a) left, (d) right")
               10 10)))

(defn my-key-press
  [state event]
  (let [jt (:joint (::things state))]
    (case (:raw-key event)
      \l (do (enable-limit! jt (not (limit-enabled? jt)))
             state)
      \m (do (enable-motor! jt (not (motor-enabled? jt)))
             state)
      \a (do (motor-speed! jt PI)
             state)
      \d (do (motor-speed! jt (- PI))
             state)
      ;; otherwise pass on to testbed
      (bed/key-press state event))))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Revolute"
   :setup setup
   :update (fn [s] (if (:paused? s) s (step s)))
   :draw draw
   :key-typed my-key-press
   :mouse-pressed bed/mouse-pressed
   :mouse-released bed/mouse-released
   :mouse-dragged bed/mouse-dragged
   :mouse-wheel bed/mouse-wheel
   :size [600 500]
   :features [:resizable]
   :middleware [quil.middleware/fun-mode]))
