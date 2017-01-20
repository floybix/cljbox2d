(ns org.nfrac.liquidfun.testbed.tests.liquid-timer
  "A translation of the LiquidFun test LiquidTimer."
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
                      {:shape (lf/edge-loop [[-2 0] [2 0] [2 4] [-2 4]])}
                      {:shape shape2}
                      {:shape shape3})
        bods [(body! world {:type :static}
                     {:shape (lf/edge [-2 3.2] [-1.2 3.2])})
              (body! world {:type :static}
                     {:shape (lf/edge [-1.1 3.2] [2 3.2])})
              (body! world {:type :static}
                     {:shape (lf/edge [-1.2 3.2] [-1.2 2.8])})
              (body! world {:type :static}
                     {:shape (lf/edge [-1.1 3.2] [-1.1 2.8])})
              (body! world {:type :static}
                     {:shape (lf/edge [-1.6 2.4] [0.8 2])})
              (body! world {:type :static}
                     {:shape (lf/edge [1.6 1.6] [-0.8 1.2])})
              (body! world {:type :static}
                     {:shape (lf/edge [-1.2 0.8] [-1.2 0])})
              (body! world {:type :static}
                     {:shape (lf/edge [-0.4 0.8] [-0.4 0])})
              (body! world {:type :static}
                     {:shape (lf/edge [0.4 0.8] [0.4 0])})
              (body! world {:type :static}
                     {:shape (lf/edge [1.2 0.8] [1.2 0])})]
        ps (particle-system! world {:radius 0.025}
                             {:shape (lf/box 2 0.4 [0 3.6])
                              :flags (lf/particle-flags #{:tensile :viscous})
                              :color [255 0 0 255]})]
    (assoc bed/initial-state
      :world world
      :dt-secs (/ 1 60.0)
      :camera (bed/map->Camera {:width 4.5 :height 4 :center [0 2.0]}))))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Liquid Timer"
   :host "liquidfun"
   :setup setup
   :update (fn [s] (if (:paused? s) s (step s)))
   ;; doesn't seem to be a way to step without drawing; manually:
   :draw #(if (zero? (mod (quil/frame-count) 2)) (bed/draw %))
   :key-typed bed/key-press
   :mouse-pressed bed/mouse-pressed
   :mouse-released bed/mouse-released
   :mouse-dragged bed/mouse-dragged
   :mouse-wheel bed/mouse-wheel
   :size [600 500]
   :features [:resizable]
   :middleware [quil.middleware/fun-mode]))
