(ns org.nfrac.liquidfun.testbed.tests.surface-tension
  "A translation of the LiquidFun test SurfaceTension."
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
        ps (particle-system! world {:radius 0.035
                                    :damping-strength 0.2}
                             {:shape (lf/circle 0.5 [0 2])
                              :flags (lf/particle-flags #{:tensile :color-mixing})
                              :color [255 0 0 255]}
                             {:shape (lf/circle 0.5 [-1 2])
                              :flags (lf/particle-flags #{:tensile :color-mixing})
                              :color [0 255 0 255]}
                             {:shape (lf/box 1 0.25)
                              :position [1 3.25]
                              :flags (lf/particle-flags #{:tensile :color-mixing})
                              :color [0 0 255 255]})
        cir (body! world {:position [0 8]}
                   {:shape (lf/circle 0.5)
                    :density 0.5})
        its (.CalculateReasonableParticleIterations world (/ 1 60.0))]
    (println "reasonable particle iterations:" its)
    (println "damping:" (.GetDamping ps))
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
   :title "Surface tension"
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
