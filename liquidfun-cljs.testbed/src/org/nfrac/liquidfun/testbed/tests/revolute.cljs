(ns org.nfrac.liquidfun.testbed.tests.revolute
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.RevoluteTest"
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf :refer [body! joint!]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(def PI Math/PI)

(defn setup []
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-40 0] [40 0])})
        w 100
        ball (body! world {:position [0 20]
                           :angular-velocity w
                           :linear-velocity [(* -8 w) 0]}
                    {:shape (lf/circle 0.5) :density 5})
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
      ::dir :clockwise
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
    (quil/text (str "Limits " (if (lf/limit-enabled? jt) "on" "off")
                    ", Motor " (if (lf/motor-enabled? jt) "on " "off ")
                    ;(if (pos? (lf/motor-speed jt)) "left" "right") "\n"
                    (name (::dir state)) "\n"
                    "Keys: (l) limits, (m) motor, (a) counter-, (d) clockwise")
               10 10)))

(defn my-key-press
  [state event]
  (let [jt (:joint (::things state))]
    (case (:key event)
      :l (do (lf/enable-limit! jt (not (lf/limit-enabled? jt)))
             state)
      :m (do (lf/enable-motor! jt (not (lf/motor-enabled? jt)))
             state)
      ;; with liquidfun.js, motor-speed! only works when it's off:
      ;; (also, doesn't work if you turn it off & on in same iteration)
      :a (do (lf/enable-motor! jt false)
             (lf/motor-speed! jt PI)
             (assoc state ::dir :countercw))
      :d (do (lf/enable-motor! jt false)
             (lf/motor-speed! jt (- PI))
             (assoc state ::dir :clockwise))
      ;; otherwise pass on to testbed
      (bed/key-press state event))))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Revolute"
   :host "liquidfun"
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
