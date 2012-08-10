(ns cljbox2d.tests.collision-processing
  (:use [cljbox2d core testbed])
  (:require [quil.core :as quil]))

;;; A translation of Daniel Murphy's
;;; org.jbox2d.testbed.tests.CollisionProcessing

(def things (atom {}))

(defn scale-vv
  "Scale a vector of vertices around [0 0]."
  [vv s]
  (map (fn [[x y]] [(* x s) (* y s)]) vv))

(defn setup-world! []
  (create-world!)
  (let [ground (body! (body-def :type :static)
                      (fixture-def (edge [-50 0] [50 0])))
        rand-pos (fn [] [(+ -5 (rand 10))
                         (+ 2 (rand 33))])
        tri-vv [[-1 0] [1 0] [0 2]]
        tri-small (body! (body-def :position (rand-pos))
                         (fixture-def (polygon tri-vv)))
        tri-big (body! (body-def :position (rand-pos))
                       (fixture-def (polygon (scale-vv tri-vv 2))))
        rect-small (body! (body-def :position (rand-pos))
                          (fixture-def (box 1 0.5)))
        rect-big (body! (body-def :position (rand-pos))
                        (fixture-def (box 2 1)))
        circ-small (body! (body-def :position (rand-pos))
                          (fixture-def (circle 1)))
        circ-big (body! (body-def :position (rand-pos))
                        (fixture-def (circle 2)))]
    (reset! things {:ground ground
                    :objs [tri-small tri-big
                           rect-small rect-big
                           circ-small circ-big]})
    (reset! ground-body ground)))

(defn setup []
  (setup-world!)
  (set-buffering-contact-listener!))

(defn draw []
  (step! (/ 1 (quil/current-frame-rate)))
  ;; process the buffer of contact points
  (let [to-nuke (for [[fixt-a fixt-b _ _] @contact-buffer]
                  (let [body-a (body fixt-a)
                        body-b (body fixt-b)
                        mass-a (mass body-a)
                        mass-b (mass body-b)]
                    (when (and (pos? mass-a) (pos? mass-b))
                      (if (< mass-a mass-b) body-a body-b))))]
    (doseq [b (remove nil? (distinct to-nuke))]
      (.destroyBody *world* b)))
  (reset! contact-buffer [])
  (draw-world))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Collision Processing"
    :setup setup
    :draw draw
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :size [600 500]))
