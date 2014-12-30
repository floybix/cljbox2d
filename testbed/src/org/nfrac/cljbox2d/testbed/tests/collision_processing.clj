(ns org.nfrac.cljbox2d.testbed.tests.collision-processing
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.CollisionProcessing"
  (:require [org.nfrac.cljbox2d.testbed :as bed :refer [the-world
                                                        dt-secs]]
            [cljbox2d.core :refer :all]
            [quil.core :as quil]))

(def things (atom {}))

(defn scale-vv
  "Scale a vector of vertices around [0 0]."
  [vv s]
  (map (fn [[x y]] [(* x s) (* y s)]) vv))

(defn setup-world! []
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-50 0] [50 0])})
        rand-pos (fn [] [(+ -5 (rand 10))
                         (+ 2 (rand 33))])
        tri-vv [[-1 0] [1 0] [0 2]]
        tri-small (body! world {:position (rand-pos)}
                         {:shape (polygon tri-vv)})
        tri-big (body! world {:position (rand-pos)}
                       {:shape (polygon (scale-vv tri-vv 2))})
        rect-small (body! world {:position (rand-pos)}
                          {:shape (box 1 0.5)})
        rect-big (body! world {:position (rand-pos)}
                        {:shape (box 2 1)})
        circ-small (body! world {:position (rand-pos)}
                          {:shape (circle 1)})
        circ-big (body! world {:position (rand-pos)}
                        {:shape (circle 2)})]
    (reset! the-world world)
    (reset! things {:objs [tri-small tri-big
                           rect-small rect-big
                           circ-small circ-big]})))

(defn post-step! []
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
  (quil/frame-rate (/ 1 @dt-secs))
  (setup-world!)
  (bed/set-buffering-contact-listener! @the-world))

(defn draw []
  (when-not @bed/paused?
    (step! @the-world @dt-secs)
    (post-step!))
  (bed/draw-world @the-world)
  (quil/fill 255)
  (quil/text "Larger bodies destroy smaller bodies on contact."
             10 10))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Collision Processing"
    :setup setup
    :draw draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]))
