(ns cljbox2d.tests.collision-processing
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.CollisionProcessing"
  (:use [cljbox2d core testbed])
  (:require [quil.core :as quil]))

(def things (atom {}))

(defn scale-vv
  "Scale a vector of vertices around [0 0]."
  [vv s]
  (map (fn [[x y]] [(* x s) (* y s)]) vv))

(defn setup-world! []
  (create-world!)
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
    (reset! ground-body ground)))

(defn my-step []
  ;; process the buffer of contact points
  (let [to-nuke (for [[fixt-a fixt-b _ _] @contact-buffer]
                  (let [body-a (body fixt-a)
                        body-b (body fixt-b)
                        mass-a (mass body-a)
                        mass-b (mass body-b)]
                    (when (and (pos? mass-a) (pos? mass-b))
                      (if (< mass-a mass-b) body-a body-b))))]
    (doseq [b (remove nil? (distinct to-nuke))]
      (destroy! b)))
  (reset! contact-buffer []))

(defn setup []
  (setup-world!)
  (set-buffering-contact-listener!)
  (reset! step-fn my-step))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Collision Processing"
    :setup setup
    :draw draw
    :key-typed key-press
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :size [600 500]))
