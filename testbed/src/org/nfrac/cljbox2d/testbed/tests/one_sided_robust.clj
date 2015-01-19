(ns org.nfrac.cljbox2d.testbed.tests.one-sided-robust
  "Based on http://www.iforce2d.net/b2dtut/one-way-walls"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d :refer [y-val v-sub]]
            [quil.core :as quil]
            [quil.middleware])
  (:import (org.jbox2d.callbacks ContactListener)
           (org.jbox2d.dynamics.contacts Contact)))

(def SPEED 1)
(def PI Math/PI)

(defn handle-one-way-contact!
  [^Contact contact platform local-top-y]
  (let [{:keys [fixture-a fixture-b points]} (contact-data contact)
        body-a (body fixture-a)
        body-b (body fixture-b)
        other-body (cond
                    (= body-a platform) body-b
                    (= body-b platform) body-a)]
    ;; check if one of the contact fixtures is the platform
    (when other-body
      ;; if any contact points are moving
      ;; downward, leave contact solid
      (when (some (fn [point]
                    (let [rel-vel (->> (v-sub (linear-velocity-world other-body point)
                                              (linear-velocity-world platform point))
                                       (to-local-vect platform))]
                      (cond
                       ;; moving down faster than -1 m/s, leave solid
                       (< (y-val rel-vel) -1)
                       false ;; leave solid
                       ;; moving up faster than 1 m/s, cancel contact
                       (> (y-val rel-vel) 1)
                       true ;; disable
                       ;; borderline case
                       :else
                       ;; contact more than 5cm inside platform?
                       (< (y-val (to-local platform point))
                          (- local-top-y 0.05)))))
                  points)
        (.setEnabled contact false)))))

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-20 10] [-20 0])}
                      {:shape (edge [-20 0] [20 0])}
                      {:shape (edge [20 0] [20 10])})
        platform (body! world {:type :kinematic
                               :position [0 10]
                               :angular-velocity SPEED}
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
         (handle-one-way-contact! contact platform plat-face-local-y))))
    (assoc bed/initial-state
      ::platform platform
      :world world)))

(defn post-step
  [state]
  (let [platform (::platform state)]
    (when (and (> (angle platform) (/ PI 2))
               (pos? (angular-velocity platform)))
      (angular-velocity! platform (- SPEED)))
    (when (and (< (angle platform) (- (/ PI 2)))
               (neg? (angular-velocity platform)))
      (angular-velocity! platform SPEED))
    state))

(defn step
  [state]
  (if (:paused? state)
    state
    (-> (update-in state [:world] step! (:dt-secs state))
        (post-step))))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "One-Sided, robust"
    :setup setup
    :update step
    :draw bed/draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]
    :middleware [quil.middleware/fun-mode]))
