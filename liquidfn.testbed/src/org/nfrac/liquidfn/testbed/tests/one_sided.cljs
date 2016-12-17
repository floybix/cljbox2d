(ns org.nfrac.liquidfn.testbed.tests.one-sided
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.OneSidedTest"
  (:require [org.nfrac.liquidfn.testbed :as bed]
            [org.nfrac.liquidfn.core :as lf :refer [body! joint!]]
            [org.nfrac.liquidfn.vec2d :refer [y-val]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(defn setup []
  (quil/frame-rate 60)
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-20 0] [20 0])})
        platform (body! world {:type :static
                               :position [0 10]}
                        {:shape (lf/box 3 0.5)})
        plat-face-local-y 0.5
        actor (body! world {:position [0 12]
                            :linear-velocity [0 -50]}
                     {:shape (lf/circle 0.5)
                      :density 20})]
    (.SetContactListener
     world
     (lf/contact-listener-callback
      {:presolve
       (fn [contact _]
         (let [{:keys [fixture-a fixture-b points]} (lf/contact-data contact)]
           ;; check if one of the fixtures is the platform
           (when (or (= platform (lf/body-of fixture-a))
                     (= platform (lf/body-of fixture-b)))
             ;; if any points are below the platform top surface, cancel contact
             (when (some (fn [point]
                           ;; contact more than 5cm inside platform?
                           (< (y-val (lf/to-local platform point))
                              (- plat-face-local-y 0.05)))
                         points)
               (.SetEnabled contact false)))))}))
    (assoc bed/initial-state
      :world world
      ;; note the initial collision is missed with dt-secs 1/30
      :dt-secs (/ 1 60.0))))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "One-Sided"
   :host "liquidfn"
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
