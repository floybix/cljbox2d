(ns org.nfrac.liquidfun.testbed.tests.collision-processing
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.CollisionProcessing"
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf :refer [body! joint!]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(defn scale-vv
  "Scale a vector of vertices around [0 0]."
  [vv s]
  (map (fn [[x y]] [(* x s) (* y s)]) vv))

(defn setup []
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-50 0] [50 0])})
        rand-pos (fn [] [(+ -5 (rand 10))
                         (+ 2 (rand 33))])
        tri-vv [[-1 0] [1 0] [0 2]]
        tri-small (body! world {:position (rand-pos)}
                         {:shape (lf/polygon tri-vv)})
        tri-big (body! world {:position (rand-pos)}
                       {:shape (lf/polygon (scale-vv tri-vv 2))})
        rect-small (body! world {:position (rand-pos)}
                          {:shape (lf/box 1 0.5)})
        rect-big (body! world {:position (rand-pos)}
                        {:shape (lf/box 2 1)})
        circ-small (body! world {:position (rand-pos)}
                          {:shape (lf/circle 1)})
        circ-big (body! world {:position (rand-pos)}
                        {:shape (lf/circle 2)})
        [lstnr contact-buffer] (lf/buffering-contact-listener)]
    (.SetContactListener world lstnr)
    (assoc bed/initial-state
      :world world
      :contact-listener lstnr
      :contact-buffer contact-buffer)))

(defn post-step
  [state]
  ;; process the buffer of contact points
  (let [cbuffer (:contact-buffer state)
        to-nuke (for [{:keys [fixture-a fixture-b]} @cbuffer]
                  (let [body-a (lf/body-of fixture-a)
                        body-b (lf/body-of fixture-b)
                        mass-a (lf/mass body-a)
                        mass-b (lf/mass body-b)]
                    (when (and (pos? mass-a) (pos? mass-b))
                      (if (< mass-a mass-b) body-a body-b))))]
    (doseq [b (remove nil? (distinct to-nuke))]
      (lf/destroy-body! b))
    (swap! cbuffer empty)
    state))

(defn step
  [state]
  (-> (bed/world-step state)
      (post-step)
      (bed/record-snapshot true)))

(defn draw
  [state]
  (bed/draw state)
  (quil/fill 255)
  (quil/text "Larger bodies destroy smaller bodies on contact."
             10 10))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Collision Processing"
   :host "liquidfun"
   :setup setup
   :update (fn [s] (if (:paused? s) s (step s)))
   :draw draw
   :key-typed bed/key-press
   :mouse-pressed bed/mouse-pressed
   :mouse-released bed/mouse-released
   :mouse-dragged bed/mouse-dragged
   :mouse-wheel bed/mouse-wheel
   :size [600 500]
   :features [:resizable]
   :middleware [quil.middleware/fun-mode]))
