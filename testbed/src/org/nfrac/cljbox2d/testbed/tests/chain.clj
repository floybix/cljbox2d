(ns org.nfrac.cljbox2d.testbed.tests.chain
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.Chain"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer :all]
            [quil.core :as quil]
            [quil.middleware]))

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        y 25.0
        linktip (reduce (fn [prev-body i]
                          (let [b (body! world {:position [(+ 0.5 i) y]}
                                         {:shape (box 0.6 0.125)
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
  (if (:paused? state)
    state
    (update-in state [:world] step! (:dt-secs state))))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Chain"
    :setup setup
    :update step
    :draw bed/draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]
    :middleware [quil.middleware/fun-mode]))
