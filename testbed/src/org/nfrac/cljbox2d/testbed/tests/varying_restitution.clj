(ns org.nfrac.cljbox2d.testbed.tests.varying-restitution
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.VaryingRestitution"
  (:require [org.nfrac.cljbox2d.testbed :as bed :refer [the-world
                                                        dt-secs]]
            [cljbox2d.core :refer :all]
            [quil.core :as quil]))

(defn setup-world! []
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        restns [0.0 0.1 0.3 0.5 0.75 0.9 1.0]
        balls (doall (for [[i r] (map-indexed list restns)]
                       (body! world {:position [(+ -10 (* 3 i)) 20]}
                              {:shape (circle 1), :restitution r})))]
    (reset! the-world world)))

(defn setup []
  (quil/frame-rate (/ 1 @dt-secs))
  (setup-world!))

(defn draw []
  (when-not @bed/paused?
    (step! @the-world @dt-secs))
  (bed/draw-world @the-world))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Varying Restitution"
    :setup setup
    :draw draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]))
