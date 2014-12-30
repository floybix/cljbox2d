(ns org.nfrac.cljbox2d.testbed.tests.sensor-test
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.SensorTest"
  (:require [org.nfrac.cljbox2d.testbed :as bed :refer [the-world
                                                        dt-secs]]
            [cljbox2d.core :refer :all]
            [cljbox2d.vec2d :refer [v-scale]]
            [quil.core :as quil])
  (:import (org.jbox2d.callbacks ContactListener)))

(def sensor (atom nil))

(def balls (atom []))

(defn setup-world! []
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        sens (fixture! ground {:shape (circle 5 [0 10])
                               :is-sensor true})
        ballseq (for [i (range 7)
                      :let [x (+ -10 (* i 3))]]
                  (body! world {:position [x 20]
                                :user-data (atom {:touching false})}
                         {:shape (circle 1)}))]
    (reset! sensor sens)
    (reset! balls (doall ballseq))
    (reset! the-world world)))

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

(defn post-step! []
  ;; process the buffer of contact points
  (let [cent (center @sensor)]
    (doseq [b @balls
            :when (:touching @(user-data b))
            :let [pt (position b)
                  d (map - cent pt)
                  d-unit (v-scale d)
                  forc (v-scale d-unit 100)]]
      (apply-force! b forc pt))))

(defn setup []
  (quil/frame-rate (/ 1 @dt-secs))
  (setup-world!)
  (.setContactListener @the-world (sensor-touching-listener)))

(defn draw []
  (when-not @bed/paused?
    (step! @the-world @dt-secs)
    (post-step!))
  (bed/draw-world @the-world))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Sensor Test"
    :setup setup
    :draw draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]))
