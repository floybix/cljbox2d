(ns org.nfrac.cljbox2d.testbed.tests.varying-restitution
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.VaryingRestitution"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [org.nfrac.cljbox2d.core :refer :all]
            [quil.core :as quil]
            [quil.middleware]))

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        restns [0.0 0.1 0.3 0.5 0.75 0.9 1.0]
        balls (doall (for [[i r] (map-indexed list restns)]
                       (body! world {:position [(+ -10 (* 3 i)) 20]}
                              {:shape (circle 1), :restitution r})))]
    (assoc bed/initial-state
      :world world)))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Varying Restitution"
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
