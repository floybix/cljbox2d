(ns org.nfrac.liquidfun.testbed.tests.elastic-particles
  "A translation of the LiquidFun test ElasticParticles."
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf :refer [body! joint!
                                                     particle-system!]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(defn setup []
  (let [world (lf/new-world)
        shape1 (lf/polygon [[-4 -1] [4 -1] [4 0] [-4 0]])
        shape2 (lf/polygon [[-4 -0.1] [-2 -0.1] [-2 2] [-4 2]])
        shape3 (lf/polygon [[2 -0.1] [4 -0.1] [4 2] [2 2]])
        ground (body! world {:type :static}
                      {:shape shape1}
                      {:shape shape2}
                      {:shape shape3})
        ps (particle-system! world {:radius 0.035}
                             {:shape (lf/circle 0.5 [0 3])
                              :flags (lf/particle-flags #{:spring})
                              :group-flags (lf/particle-group-flags #{:solid})
                              :color [255 0 0 255]}
                             {:shape (lf/circle 0.5 [-1 3])
                              :flags (lf/particle-flags #{:elastic})
                              :group-flags (lf/particle-group-flags #{:solid})
                              :color [0 255 0 255]}
                             {:shape (lf/box 1 0.5)
                              :position [1 4]
                              :angle -0.5
                              :angular-velocity 2
                              :flags (lf/particle-flags #{:elastic})
                              :group-flags (lf/particle-group-flags #{:solid})
                              :color [128 128 255 255]})
        cir (body! world {:position [0 8]}
                   {:shape (lf/circle 0.5)
                    :density 0.5})
        its (.CalculateReasonableParticleIterations world (/ 1 60.0))]
    (println "reasonable particle iterations:" its)
    (assoc bed/initial-state
      :world world
      :particle-iterations its
      :dt-secs (/ 1 60.0)
      :camera (bed/map->Camera {:width 6 :height 5 :center [0 2]}))))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Elastic particles"
   :host "liquidfun"
   :setup setup
   :update (fn [s] (if (:paused? s) s (step s)))
   :draw #(bed/draw % true)
   :key-typed bed/key-press
   :mouse-pressed bed/mouse-pressed
   :mouse-released bed/mouse-released
   :mouse-dragged bed/mouse-dragged
   :mouse-wheel bed/mouse-wheel
   :size [600 500]
   :features [:resizable]
   :middleware [quil.middleware/fun-mode]))
