(ns org.nfrac.cljbox2d.testbed.tests.slider-crank
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.SliderCrankTest"
  (:require [org.nfrac.cljbox2d.testbed :as bed :refer [the-world
                                                        dt-secs]]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [cljbox2d.vec2d :refer [PI]]
            [quil.core :as quil]))

(def things (atom {}))

(defn setup-world! []
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        crank (body! world {:position [0 7]}
                     {:shape (box 0.5 2.0) :density 2})
        crank-j (revolute-joint! world ground crank [0 5]
                              {:motor-speed PI
                               :max-motor-torque 10000
                               :enable-motor true})
        follow (body! world {:position [0 13]}
                      {:shape (box 0.5 4.0) :density 2})
        follow-j (revolute-joint! world crank follow [0 9]
                                  {:enable-motor false})
        piston (body! world {:position [0 17]}
                      {:shape (box 1.5 1.5) :density 2})
        piston-rj (revolute-joint! world follow piston [0 17] {})
        piston-pj (prismatic-joint! world ground piston [0 17] [0 1]
                                    {:max-motor-force 1000
                                     :enable-motor true})
        payload (body! world {:position [0 23]}
                       {:shape (box 1.5 1.5) :density 2})]
    (reset! the-world world)
    (reset! things {:crank-j crank-j
                    :piston-pj piston-pj})))

(defn my-key-press []
  (let [cj (:crank-j @things)
        pj (:piston-pj @things)]
    (case (quil/raw-key)
      \f (enable-motor! pj (not (motor-enabled? pj)))
      \m (enable-motor! cj (not (motor-enabled? cj)))
      ;; otherwise pass on to testbed
      (bed/key-press))))

(defn setup []
  (quil/frame-rate (/ 1 @dt-secs))
  (setup-world!))

(defn draw []
  (when-not @bed/paused?
    (step! @the-world @dt-secs))
  (bed/draw-world @the-world)
  (quil/fill 255)
  (quil/text "Keys: (f) toggle friction, (m) toggle motor"
             10 10))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Slider Crank"
    :setup setup
    :draw draw
    :key-typed my-key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]))
