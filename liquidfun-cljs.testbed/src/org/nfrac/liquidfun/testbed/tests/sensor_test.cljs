(ns org.nfrac.liquidfun.testbed.tests.sensor-test
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.SensorTest"
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf :refer [body! joint!]]
            [org.nfrac.liquidfun.vec2d :refer [v-scale]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(defn sensor-touching-listener
  "Checks for any bodies touching the given sensor fixture, setting
   boolean key :touching? in their body user-data."
  [sensor-fixt]
  {:begin-contact
   (fn [contact]
      (let [fixt-a (.GetFixtureA contact)
            fixt-b (.GetFixtureB contact)
            bod (cond
                 (= fixt-a sensor-fixt) (lf/body-of fixt-b)
                 (= fixt-b sensor-fixt) (lf/body-of fixt-a))]
        (when bod
          (lf/vary-user-data bod assoc ::touching? true ::bed/rgb [255 0 0]))))
   :end-contact
   (fn [contact]
      (let [fixt-a (.GetFixtureA contact)
            fixt-b (.GetFixtureB contact)
            bod (cond
                 (= fixt-a sensor-fixt) (lf/body-of fixt-b)
                 (= fixt-b sensor-fixt) (lf/body-of fixt-a))]
        (when bod
          (lf/vary-user-data bod assoc ::touching? false ::bed/rgb nil))))})

(defn setup []
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-40 0] [40 0])})
        sens (body! world {:type :static
                           :position [0 10]}
                    {:shape (lf/circle 5 [0 0])
                     :is-sensor true})
        ballseq (for [i (range 7)
                      :let [x (+ -10 (* i 3))]]
                  (body! world {:position [x 20]
                                :user-data {::touching? false}}
                         {:shape (lf/circle 1)}))]
    (.SetContactListener world
                         (lf/contact-listener-callback
                          (sensor-touching-listener (lf/fixture-of sens))))
    (assoc bed/initial-state
      :world world
      ::things {:balls (doall ballseq) :sensor sens})))

(defn post-step
  [state]
  ;; process the buffer of contact points
  (let [sens (:sensor (::things state))
        cent (lf/center sens)]
    (doseq [b (:balls (::things state))
            :when (::touching? (lf/user-data b))
            :let [pt (lf/position b)
                  d (map - cent pt)
                  d-unit (v-scale d)
                  forc (v-scale d-unit 100)]]
      (lf/apply-force! b forc pt))
    state))

(defn step
  [state]
  (-> (bed/world-step state)
      (post-step)
      (bed/record-snapshot true)))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Sensor Test"
   :host "liquidfun"
   :setup setup
   :update (fn [s] (if (:paused? s) s (step s)))
   :draw bed/draw
   :key-typed bed/key-press
   :mouse-pressed bed/mouse-pressed
   :mouse-released bed/mouse-released
   :mouse-dragged bed/mouse-dragged
   :mouse-wheel bed/mouse-wheel
   :size [600 500]
   :features [:resizable]
   :middleware [quil.middleware/fun-mode]))
