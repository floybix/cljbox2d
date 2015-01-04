(ns org.nfrac.cljbox2d.testbed.tests.collision-processing
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.CollisionProcessing"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer :all]
            [quil.core :as quil]
            [quil.middleware]))

(defn scale-vv
  "Scale a vector of vertices around [0 0]."
  [vv s]
  (map (fn [[x y]] [(* x s) (* y s)]) vv))

(defn setup []
  (quil/frame-rate 30)
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
    (assoc bed/initial-state
      :world world
      :contact-buffer (set-buffering-contact-listener! world))))

(defn post-step
  [state]
  ;; process the buffer of contact points
  (let [cbuffer (:contact-buffer state)
        to-nuke (for [{:keys [fixture-a fixture-b]} @cbuffer]
                  (let [body-a (body fixture-a)
                        body-b (body fixture-b)
                        mass-a (mass body-a)
                        mass-b (mass body-b)]
                    (when (and (pos? mass-a) (pos? mass-b))
                      (if (< mass-a mass-b) body-a body-b))))]
    (doseq [b (remove nil? (distinct to-nuke))]
      (destroy! b))
    (swap! cbuffer empty)
    state))

(defn step
  [state]
  (if (:paused? state)
    state
    (-> (update-in state [:world] step! (:dt-secs state))
        (post-step))))

(defn draw
  [state]
  (bed/draw state)
  (quil/fill 255)
  (quil/text "Larger bodies destroy smaller bodies on contact."
             10 10))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Collision Processing"
    :setup setup
    :update step
    :draw draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]
    :middleware [quil.middleware/fun-mode]))
