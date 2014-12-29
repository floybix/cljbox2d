(ns org.nfrac.cljbox2d.testbed.tests.slider-crank
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.SliderCrankTest"
  (:require [org.nfrac.cljbox2d.testbed :as bed :refer [*timestep*]]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [cljbox2d.vec2d :refer [PI]]
            [quil.core :as quil]))

(def things (atom {}))

(defn setup-world! []
  (reset-world! (new-world))
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
    (reset! bed/ground-body ground)))

(defn update-info-text []
  (let [jt (:joint @things)]
    (reset! bed/info-text
            (str "Keys: (f) toggle friction, (m) toggle motor"))))

(defn my-key-press []
  (let [cj (:crank-j @things)
        pj (:piston-pj @things)]
    (case (quil/raw-key)
      \f (enable-motor! pj (not (motor-enabled? pj)))
      \m (enable-motor! cj (not (motor-enabled? cj)))
      ;; otherwise pass on to testbed
      (bed/key-press)))
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
    :draw bed/draw
    :key-typed my-key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]))
