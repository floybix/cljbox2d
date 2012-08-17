(ns cljbox2d.tests.sensor-test
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.SensorTest"
  (:use [cljbox2d core testbed])
  (:import (org.jbox2d.callbacks ContactListener))
  (:require [quil.core :as quil]))

(def sensor (atom nil))

(def balls (atom []))

(defn setup-world! []
  (create-world!)
  (let [ground (body! (body-def :type :static)
                      (fixture-def (edge [-40 0] [40 0])))
        sens (fixture! ground (circle 5 [0 10])
                       :is-sensor true)
        ballseq (for [i (range 7)
                      :let [x (+ -10 (* i 3))]]
                  (body! (body-def :position [x 20]
                                   :user-data (atom {:touching false}))
                         (fixture-def (circle 1))))]
    (reset! sensor sens)
    (reset! balls (doall ballseq))
    (reset! ground-body ground)))

(defn sensor-touching-listener
  []
  (reify ContactListener
    (beginContact [_ contact]
      (let [fixt-a (.getFixtureA contact)
            fixt-b (.getFixtureB contact)
            bod (cond
                 (= fixt-a @sensor) (body fixt-b)
                 (= fixt-b @sensor) (body fixt-a))]
        (when bod
          (swap! (user-data bod) assoc-in [:touching] true))))
    (endContact [_ contact]
      (let [fixt-a (.getFixtureA contact)
            fixt-b (.getFixtureB contact)
            bod (cond
                 (= fixt-a @sensor) (body fixt-b)
                 (= fixt-b @sensor) (body fixt-a))]
        (when bod
          (swap! (user-data bod) assoc-in [:touching] false))))
    (postSolve [_ contact impulse])
    (preSolve [_ contact omanifold])))

(defn setup []
  (setup-world!)
  (.setContactListener *world* (sensor-touching-listener)))

(defn draw []
  (step! (/ 1 (quil/current-frame-rate)))
  ;; process the buffer of contact points
  (let [cent (first (world-coords @sensor))]
    (doseq [b @balls
            :when (:touching @(user-data b))
            :let [pt (world-point b)
                  d (map - cent pt)
                  d-unit (scale-v d)
                  forc (scale-v d-unit 100)]]
      (apply-force! b forc pt)))
  (draw-world))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Sensor Test"
    :setup setup
    :draw draw
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :size [600 500]))
