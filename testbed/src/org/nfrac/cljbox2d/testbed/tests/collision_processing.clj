(ns org.nfrac.cljbox2d.testbed.tests.collision-processing
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.CollisionProcessing"
  (:require [cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.testbed :as bed :refer [*timestep*]]
            [quil.core :as quil]))

(def things (atom {}))

(defn scale-vv
  "Scale a vector of vertices around [0 0]."
  [vv s]
  (map (fn [[x y]] [(* x s) (* y s)]) vv))

(defn setup-world! []
  (reset-world! (new-world))
  (let [ground (body! {:type :static}
                      {:shape (edge [-50 0] [50 0])})
        rand-pos (fn [] [(+ -5 (rand 10))
                         (+ 2 (rand 33))])
        tri-vv [[-1 0] [1 0] [0 2]]
        tri-small (body! {:position (rand-pos)}
                         {:shape (polygon tri-vv)})
        tri-big (body! {:position (rand-pos)}
                       {:shape (polygon (scale-vv tri-vv 2))})
        rect-small (body! {:position (rand-pos)}
                          {:shape (box 1 0.5)})
        rect-big (body! {:position (rand-pos)}
                        {:shape (box 2 1)})
        circ-small (body! {:position (rand-pos)}
                          {:shape (circle 1)})
        circ-big (body! {:position (rand-pos)}
                        {:shape (circle 2)})]
    (reset! things {:objs [tri-small tri-big
                           rect-small rect-big
                           circ-small circ-big]})
    (reset! bed/ground-body ground)))

(defn my-step []
  ;; process the buffer of contact points
  (let [to-nuke (for [[fixt-a fixt-b _ _] @bed/contact-buffer]
                  (let [body-a (body fixt-a)
                        body-b (body fixt-b)
                        mass-a (mass body-a)
                        mass-b (mass body-b)]
                    (when (and (pos? mass-a) (pos? mass-b))
                      (if (< mass-a mass-b) body-a body-b))))]
    (doseq [b (remove nil? (distinct to-nuke))]
      (destroy! b)))
  (reset! bed/contact-buffer []))

(defn setup []
  (quil/frame-rate (/ 1 *timestep*))
  (setup-world!)
  (bed/set-buffering-contact-listener!)
  (reset! bed/step-fn my-step))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Collision Processing"
    :setup setup
    :draw bed/draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]))
