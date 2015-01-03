(ns org.nfrac.cljbox2d.testbed.tests.raycast
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.RayCastTest"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer :all]
            [cljbox2d.vec2d :refer [polar-xy v-add TWOPI PI]]
            [quil.core :as quil]
            [quil.middleware])
  (:import (org.jbox2d.callbacks ContactListener)))

(def length 11.0)
(def eye [0 10])

(def shapes
  [(polygon [[-0.5 0] [0.5 0] [0 1.5]])
   (polygon [[-0.1 0] [0.1 0] [0 1.5]])
   (polygon (for [i (range 8)
                  :let [angle (* TWOPI (/ i 8))]]
              (polar-xy 0.5 angle)))
   (box 0.5 0.5)
   (circle 0.5)])

(defn setup []
  (quil/frame-rate 60)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})]
    (assoc bed/initial-state
      :world world
      ::bodies []
      ::raycast {:mode :closest
                 :angle 0.0
                 :end-point eye})))

(defn post-step
  [state]
  (let [rc (::raycast state)
        prev-angle (:angle rc)
        angle (+ prev-angle (* 0.25 (/ PI 180)))
        end-point (v-add eye (polar-xy length angle))
        hits (raycast (:world state) eye end-point (:mode rc)
                      :ignore (fn [fixt]
                                (= 0 (:shape-idx (user-data (body fixt))))))]
    (assoc state ::raycast
           (assoc rc
             :angle angle
             :end-point end-point
             :hits hits))))

(defn step
  [state]
  (if (:paused? state)
    state
    (-> (update-in state [:world] step! (:dt-secs state))
        (post-step))))

(defn draw
  [state]
  (bed/draw state)
  (let [cam (:camera state)
        ->px (partial bed/world-to-px cam)
        rc (::raycast state)
        mode (:mode rc)
        hits (:hits rc)]
    (quil/fill 255)
    (quil/text (str "Press 1-5 to drop stuff, m to change the mode.\n"
                    "Polygon 1 is filtered.\n"
                    "Mode = " mode)
               10 10)
    (when (or (empty? hits)
              (= mode :all))
      (quil/line (->px eye)
                 (->px (:end-point rc))))    
    (doseq [hit hits]
      (quil/line (->px eye) (->px (:point hit)))
      (let [[x-px y-px] (->px (:point hit))]
        (quil/ellipse x-px y-px 5 5)))))

(defn create-body
  [state index]
  (let [x (- (rand 20) 10)
        y (rand 20)
        theta (- (rand TWOPI) PI)
        bod (body! (:world state) {:type :static
                                   :position [x y]
                                   :angle theta
                                   :user-data {:shape-idx index}}
                   {:shape (get shapes index)})]
    (update-in state [::bodies] conj bod)))

(defn my-key-press
  [state event]
  (let [char (:raw-key event)]
    (case char
      (\1 \2 \3 \4 \5)
      (let [idx (dec (Integer. (str char)))]
        (create-body state idx))
      \m (let [mode (:mode (::raycast state))
               new-mode (case mode
                          :closest :all
                          :all :closest)]
           (assoc-in state [::raycast :mode] new-mode))
      \d (do (doseq [b (::bodies state)] (destroy! b))
             (assoc state ::bodies []))
      ;; otherwise pass on to testbed
      (bed/key-press state event))))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Raycast"
    :setup setup
    :update step
    :draw draw
    :key-typed my-key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]
    :middleware [quil.middleware/fun-mode]))
