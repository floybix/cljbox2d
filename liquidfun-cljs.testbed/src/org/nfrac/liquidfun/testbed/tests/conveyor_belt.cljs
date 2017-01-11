(ns org.nfrac.liquidfun.testbed.tests.conveyor-belt
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.ConveyorBelt"
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf :refer [body! joint!]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(defn setup []
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-20 0] [20 0])})
        platform (body! world {:type :static
                               :position [-5 5]}
                        {:shape (lf/box 10 0.5)
                         :friction 0.8})
        boxes (doall (for [i (range 5)]
                       (body! world {:position [(+ -10 (* i 2)) 7]}
                              {:shape (lf/box 0.5 0.5)
                               :density 20})))]
    (.SetContactListener
     world
     (lf/contact-listener-callback
       {:presolve
        (fn [contact _]
         (let [{:keys [fixture-a fixture-b]} (lf/contact-data contact)]
           ;; check if one of the fixtures is the platform
           (when (or (= platform (lf/body-of fixture-a))
                     (= platform (lf/body-of fixture-b)))
             (.SetTangentSpeed contact 5))))}))
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
   :title "Conveyor Belt"
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
