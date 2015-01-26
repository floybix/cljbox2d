(ns org.nfrac.cljbox2d.testbed.tests.one-sided
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.OneSidedTest"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d :refer [y-val]]
            [quil.core :as quil]
            [quil.middleware])
  (:import (org.jbox2d.callbacks ContactListener)))

(defn setup []
  (quil/frame-rate 60)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-20 0] [20 0])})
        platform (body! world {:type :static
                               :position [0 10]}
                        {:shape (box 3 0.5)})
        plat-face-local-y 0.5
        actor (body! world {:position [0 12]
                            :linear-velocity [0 -50]}
                     {:shape (circle 0.5)
                      :density 20})]
    (.setContactListener
     world
     (reify ContactListener
       (beginContact [_ _])
       (endContact [_ _])
       (postSolve [_ _ _])
       (preSolve [_ contact _]
         (let [{:keys [fixture-a fixture-b points]} (contact-data contact)]
           ;; check if one of the fixtures is the platform
           (when (or (= platform (body-of fixture-a))
                     (= platform (body-of fixture-b)))
             ;; if any points are below the platform top surface, cancel contact
             (when (some (fn [point]
                           ;; contact more than 5cm inside platform?
                           (< (y-val (to-local platform point))
                              (- plat-face-local-y 0.05)))
                         points)
               (.setEnabled contact false)))))))
    (assoc bed/initial-state
      :world world
      ;; note the initial collision is missed with dt-secs 1/30
      :dt-secs (/ 1 60.0))))

(defn step
  [state]
  (if (:paused? state)
    state
    (update-in state [:world] step! (:dt-secs state))))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "One-Sided"
   :setup setup
   :update step
   :draw bed/draw
   :key-typed bed/key-press
   :mouse-pressed bed/mouse-pressed
   :mouse-released bed/mouse-released
   :mouse-dragged bed/mouse-dragged
   :mouse-wheel bed/mouse-wheel
   :size [600 500]
   :features [:resizable]
   :middleware [quil.middleware/fun-mode]))
