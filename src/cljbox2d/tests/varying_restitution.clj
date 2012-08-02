(ns cljbox2d.tests.varying-restitution
  (:use [cljbox2d core testbed])
  (:require [quil.core :as quil]))

;;; A translation of
;;; org.jbox2d.testbed.tests.VaryingRestitution

(def things)

(defn setup-world! []
  (create-world!)
  (let [ground (body! (body-def :type :static)
                      (fixture-def (edge [-40 0] [40 0])))
        restns [0.0 0.1 0.3 0.5 0.75 0.9 1.0]
        balls (doall (for [[i r] (map-indexed list restns)]
                       (body! (body-def :position [(+ -10 (* 3 i)) 20])
                              (fixture-def (circle 1) :restitution r))))]
    (def things {:ground ground :balls balls})))

(defn setup []
  (setup-style)
  (setup-world!))

(defn draw []
  (step! (/ 1 (quil/current-frame-rate)))
  (quil/background 0)
  (draw-world))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Varying Restitution"
    :setup setup
    :draw draw
    :size [600 500]))
