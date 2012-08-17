(ns cljbox2d.tests.varying-restitution
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.VaryingRestitution"
  (:use [cljbox2d core testbed])
  (:require [quil.core :as quil]))

(def things (atom {}))

(defn setup-world! []
  (create-world!)
  (let [ground (body! (body-def :type :static)
                      (fixture-def (edge [-40 0] [40 0])))
        restns [0.0 0.1 0.3 0.5 0.75 0.9 1.0]
        balls (doall (for [[i r] (map-indexed list restns)]
                       (body! (body-def :position [(+ -10 (* 3 i)) 20])
                              (fixture-def (circle 1) :restitution r))))]
    (reset! things {:ground ground :balls balls})
    (reset! ground-body ground)))

(defn setup []
  (setup-world!))

(defn draw []
  (step! (/ 1 (quil/current-frame-rate)))
  (draw-world))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Varying Restitution"
    :setup setup
    :draw draw
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :size [600 500]))
