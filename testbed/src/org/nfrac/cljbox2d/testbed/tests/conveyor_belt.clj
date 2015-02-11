(ns org.nfrac.cljbox2d.testbed.tests.conveyor-belt
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.ConveyorBelt"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [org.nfrac.cljbox2d.core :refer :all]
            [quil.core :as quil]
            [quil.middleware])
  (:import (org.jbox2d.callbacks ContactListener)))

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-20 0] [20 0])})
        platform (body! world {:type :static
                               :position [-5 5]}
                        {:shape (box 10 0.5)
                         :friction 0.8})
        boxes (doall (for [i (range 5)]
                       (body! world {:position [(+ -10 (* i 2)) 7]}
                              {:shape (box 0.5 0.5)
                               :density 20})))]
    (.setContactListener
     world
     (reify ContactListener
       (beginContact [_ _])
       (endContact [_ _])
       (postSolve [_ _ _])
       (preSolve [_ contact _]
         (let [{:keys [fixture-a fixture-b]} (contact-data contact)]
           ;; check if one of the fixtures is the platform
           (when (or (= platform (body-of fixture-a))
                     (= platform (body-of fixture-b)))
             (.setTangentSpeed contact 5))))))
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
   :title "Conveyor Belt"
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
