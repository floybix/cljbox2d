(ns cljbox2d.tests.slider-crank
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.SliderCrankTest"
  (:use (cljbox2d core joints testbed)
        [cljbox2d.vec2d :only [PI]])
  (:require [quil.core :as quil]))

(def things (atom {}))

(defn setup-world! []
  (create-world!)
  (let [ground (body! {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        crank (body! {:position [0 7]}
                     {:shape (box 0.5 2.0) :density 2})
        crank-j (revolute-joint! ground crank [0 5]
                              {:motor-speed PI
                               :max-motor-torque 10000
                               :enable-motor true})
        follow (body! {:position [0 13]}
                      {:shape (box 0.5 4.0) :density 2})
        follow-j (revolute-joint! crank follow [0 9]
                                  {:enable-motor false})
        piston (body! {:position [0 17]}
                      {:shape (box 1.5 1.5) :density 2})
        piston-rj (revolute-joint! follow piston [0 17] {})
        piston-pj (prismatic-joint! ground piston [0 17] [0 1]
                                    {:max-motor-force 1000
                                     :enable-motor true})
        payload (body! {:position [0 23]}
                       {:shape (box 1.5 1.5) :density 2})]
    (reset! things {:crank-j crank-j
                    :piston-pj piston-pj})
    (reset! ground-body ground)))

(defn update-info-text []
  (let [jt (:joint @things)]
    (reset! info-text
            (str "Keys: (f) toggle friction, (m) toggle motor"))))

(defn my-key-press []
  (let [cj (:crank-j @things)
        pj (:piston-pj @things)]
    (case (quil/raw-key)
      \f (enable-motor! pj (not (motor-enabled? pj)))
      \m (enable-motor! cj (not (motor-enabled? cj)))
      ;; otherwise pass on to testbed
      (key-press)))
  (update-info-text))

(defn setup []
  (quil/frame-rate (/ 1 *timestep*))
  (setup-world!)
  (update-info-text))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Slider Crank"
    :setup setup
    :draw draw
    :key-typed my-key-press
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :size [600 500]))
