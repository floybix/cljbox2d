(ns org.nfrac.cljbox2d.testbed.tests.sensor-test
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.SensorTest"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer :all]
            [cljbox2d.vec2d :refer [v-scale]]
            [quil.core :as quil]
            [quil.middleware])
  (:import (org.jbox2d.callbacks ContactListener)))

(defn sensor-touching-listener
  "Checks for any bodies touching the given sensor fixture, setting
   boolean key :touching? in their body user-data."
  [sensor-fixt]
  (reify ContactListener
    (beginContact [_ contact]
      (let [fixt-a (.getFixtureA contact)
            fixt-b (.getFixtureB contact)
            bod (cond
                 (= fixt-a sensor-fixt) (body fixt-b)
                 (= fixt-b sensor-fixt) (body fixt-a))]
        (when bod
          (swap! (user-data bod) assoc :touching? true :rgb [255 0 0]))))
    (endContact [_ contact]
      (let [fixt-a (.getFixtureA contact)
            fixt-b (.getFixtureB contact)
            bod (cond
                 (= fixt-a sensor-fixt) (body fixt-b)
                 (= fixt-b sensor-fixt) (body fixt-a))]
        (when bod
          (swap! (user-data bod) assoc :touching? false :rgb nil))))
    (preSolve [_ contact _])
    (postSolve [_ contact _])))

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        sens (fixture! ground {:shape (circle 5 [0 10])
                               :is-sensor true})
        ballseq (for [i (range 7)
                      :let [x (+ -10 (* i 3))]]
                  (body! world {:position [x 20]
                                :user-data (atom {:touching? false})}
                         {:shape (circle 1)}))]
    (.setContactListener world (sensor-touching-listener sens))
    (assoc bed/initial-state
      :world world
      ::things {:balls (doall ballseq) :sensor sens})))

(defn post-step
  [state]
  ;; process the buffer of contact points
  (let [sens (:sensor (::things state))
        cent (center sens)]
    (doseq [b (:balls (::things state))
            :when (:touching? @(user-data b))
            :let [pt (position b)
                  d (map - cent pt)
                  d-unit (v-scale d)
                  forc (v-scale d-unit 100)]]
      (apply-force! b forc pt))
    state))

(defn step
  [state]
  (if (:paused? state)
    state
    (-> (update-in state [:world] step! (:dt-secs state))
        (post-step))))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Sensor Test"
    :setup setup
    :update step
    :draw bed/draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]
    :middleware [quil.middleware/fun-mode]))
