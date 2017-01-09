(ns org.nfrac.liquidfun.testbed.tests.chain
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.Chain"
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf :refer [body! joint!]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(defn setup []
  (quil/frame-rate 30)
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-40 0] [40 0])})
        y 25.0
        linktip (reduce (fn [prev-body i]
                          (let [b (body! world {:position [(+ 0.5 i) y]}
                                         {:shape (lf/box 0.6 0.125)
                                          :density 20
                                          :friction 0.2})]
                            (joint! {:type :revolute
                                     :body-a prev-body
                                     :body-b b
                                     :world-anchor [i y]
                                     :collide-connected false})
                            b))
                        ground
                        (range 30))]
    (assoc bed/initial-state
      :world world)))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Chain"
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
