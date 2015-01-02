(ns org.nfrac.cljbox2d.testbed.tests.slider-crank
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.SliderCrankTest"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [cljbox2d.vec2d :refer [PI]]
            [quil.core :as quil]
            [quil.middleware]))

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        crank (body! world {:position [0 7]}
                     {:shape (box 0.5 2.0) :density 2})
        crank-j (joint! {:type :revolute
                         :body-a ground
                         :body-b crank
                         :world-anchor [0 5]
                         :motor-speed PI
                         :max-motor-torque 10000
                         :enable-motor true})
        follow (body! world {:position [0 13]}
                      {:shape (box 0.5 4.0) :density 2})
        follow-j (joint! {:type :revolute
                          :body-a crank
                          :body-b follow
                          :world-anchor [0 9]
                          :enable-motor false})
        piston (body! world {:position [0 17]}
                      {:shape (box 1.5 1.5) :density 2})
        piston-rj (joint! {:type :revolute
                           :body-a follow
                           :body-b piston
                           :world-anchor [0 17]})
        piston-pj (joint! {:type :prismatic
                           :body-a ground
                           :body-b piston
                           :world-anchor [0 17]
                           :world-axis [0 1]
                           :max-motor-force 1000
                           :enable-motor true})
        payload (body! world {:position [0 23]}
                       {:shape (box 1.5 1.5) :density 2})]
    (assoc bed/initial-state
      :world world
      ::things {:crank-j crank-j
                :piston-pj piston-pj})))

(defn my-key-press
  [state event]
  (let [things (::things state)
        cj (:crank-j things)
        pj (:piston-pj things)]
    (case (:raw-key event)
      \f (do (enable-motor! pj (not (motor-enabled? pj)))
             state)
      \m (do (enable-motor! cj (not (motor-enabled? cj)))
             state)
      ;; otherwise pass on to testbed
      (bed/key-press state event))))

(defn step
  [state]
  (if (:paused? state)
    state
    (update-in state [:world] step! (:dt-secs state))))

(defn draw
  [state]
  (bed/draw state)
  (quil/fill 255)
  (quil/text "Keys: (f) toggle friction, (m) toggle motor"
             10 10))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Slider Crank"
    :setup setup
    :update step
    :draw draw
    :key-typed my-key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]
    :middleware [quil.middleware/fun-mode]))
