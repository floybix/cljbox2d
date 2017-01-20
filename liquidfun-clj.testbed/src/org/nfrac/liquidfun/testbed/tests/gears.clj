(ns org.nfrac.liquidfun.testbed.tests.gears
  "A translation of the testbed Gears test."
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf :refer [body! joint!]]
            [quil.core :as quil :include-macros true]
            [quil.middleware])
  (:import (org.bytedeco.javacpp
            liquidfun$b2RevoluteJoint
            liquidfun$b2PrismaticJoint
            liquidfun$b2GearJoint)))

(def PI Math/PI)

(defn my-radius [body] (lf/fixture-radius (lf/fixture-of body)))

(defn setup-first-thing
  [world ground]
  (let [b1 (body! world {:type :static
                         :position [10 9]}
                  {:shape (lf/circle 1.0) :density 5})
        b2 (body! world {:position [10 8]}
                  {:shape (lf/box 0.5 5) :density 5})
        b3 (body! world {:position [10 6]}
                  {:shape (lf/circle 2.0) :density 5})
        j1 (joint! {:type :revolute
                    :body-a b2
                    :body-b b1
                    :world-anchor (lf/position b1)})
        j2 (joint! {:type :revolute
                    :body-a b2
                    :body-b b3
                    :world-anchor (lf/position b3)})
        gj (joint! {:type :gear
                    :body-a b1
                    :body-b b3
                    :joint-1 j1
                    :joint-2 j2
                    :ratio (/ (my-radius b3)
                              (my-radius b1))})]
    {:gj gj}))

(defn setup-second-thing
  [world ground]
  (let [b1 (body! world {:position [-3 12]}
                  {:shape (lf/circle 1.0) :density 5})
        j1 (joint! {:type :revolute
                    :body-a ground
                    :body-b b1
                    :world-anchor (lf/position b1)
                    :reference-angle (- (lf/angle b1)
                                        (lf/angle ground))})
        b2 (body! world {:position [0 12]}
                  {:shape (lf/circle 2.0) :density 5})
        j2 (joint! {:type :revolute
                    :body-a ground
                    :body-b b2
                    :world-anchor (lf/position b2)})
        b3 (body! world {:position [2.5 12]}
                  {:shape (lf/box 0.5 5) :density 5})
        pj (joint! {:type :prismatic
                    :body-a ground
                    :body-b b3
                    :world-anchor (lf/position b3)
                    :world-axis [0 1]
                    :lower-trans -5
                    :upper-trans 5
                    :enable-limit true})
        gj1 (joint! {:type :gear
                     :body-a b1
                     :body-b b2
                     :joint-1 j1
                     :joint-2 j2
                     :ratio (/ (my-radius b2)
                               (my-radius b1))})
        gj2 (joint! {:type :gear
                     :body-a b2
                     :body-b b3
                     :joint-1 j2
                     :joint-2 pj
                     :ratio (/ -1 (my-radius b2))})]
    {:gj1 gj1
     :gj2 gj2
     :j1 j1
     :j2 j2
     :pj pj}))

(defn setup []
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-50 0] [50 0])})
        thing1 (setup-first-thing world ground)
        thing2 (setup-second-thing world ground)]
    (assoc bed/initial-state
      :world world
      ::thing1 thing1
      ::thing2 thing2)))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn draw
  [state]
  (bed/draw state)
  (let [{:keys [j1 j2 pj gj1 gj2]} (::thing2 state)
        j1 ^liquidfun$b2RevoluteJoint j1
        j2 ^liquidfun$b2RevoluteJoint j2
        pj ^liquidfun$b2PrismaticJoint pj
        gj1 ^liquidfun$b2GearJoint gj1
        gj2 ^liquidfun$b2GearJoint gj2
        r1 (.GetRatio gj1)
        r2 (.GetRatio gj2)]
    (quil/fill 255)
    (quil/text (str (format "theta1 + %4.2f * theta2 = %4.2f\n" r1
                            (+ (.GetJointAngle j1) (* r1 (.GetJointAngle j2))))
                    (format "theta2 + %4.2f * delta = %4.2f" r2
                            (+ (.GetJointAngle j2) (* r2 (.GetJointTranslation pj)))))
               10 10)))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Gears"
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
