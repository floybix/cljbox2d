(ns org.nfrac.liquidfn.testbed.tests.slider-crank
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.SliderCrankTest"
  (:require [org.nfrac.liquidfn.testbed :as bed]
            [org.nfrac.liquidfn.core :as lf :refer [body! joint!]]
            [org.nfrac.liquidfn.vec2d :refer [PI]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(defn setup []
  (quil/frame-rate 30)
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-40 0] [40 0])})
        crank (body! world {:position [0 7]}
                     {:shape (lf/box 0.5 2.0) :density 2})
        crank-j (joint! {:type :revolute
                         :body-a ground
                         :body-b crank
                         :world-anchor [0 5]
                         :motor-speed PI
                         :max-motor-torque 10000
                         :enable-motor true})
        follow (body! world {:position [0 13]}
                      {:shape (lf/box 0.5 4.0) :density 2})
        follow-j (joint! {:type :revolute
                          :body-a crank
                          :body-b follow
                          :world-anchor [0 9]
                          :enable-motor false})
        piston (body! world {:position [0 17]}
                      {:shape (lf/box 1.5 1.5) :density 2})
        piston-rj (joint! {:type :revolute
                           :body-a follow
                           :body-b piston
                           :world-anchor [0 17]})
        piston-pj (joint! {:type :prismatic
                           :body-a ground
                           :body-b piston
                           :world-anchor [0 17]
                           :world-axis [0 1]
                           :max-motor-force 1000
                           :enable-motor true})
        payload (body! world {:position [0 23]}
                       {:shape (lf/box 1.5 1.5) :density 2})]
    (assoc bed/initial-state
      :world world
      ::things {:crank-j crank-j
                :piston-pj piston-pj})))

(defn my-key-press
  [state event]
  (let [things (::things state)
        cj (:crank-j things)
        pj (:piston-pj things)]
    (case (:key event)
      :f (do (lf/enable-motor! pj (not (lf/motor-enabled? pj)))
             state)
      :m (do (lf/enable-motor! cj (not (lf/motor-enabled? cj)))
             state)
      ;; otherwise pass on to testbed
      (bed/key-press state event))))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn draw
  [state]
  (bed/draw state)
  (quil/fill 255)
  (quil/text "Keys: (f) toggle friction, (m) toggle motor"
             10 10))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Slider Crank"
   :host "liquidfn"
   :setup setup
   :update (fn [s] (if (:paused? s) s (step s)))
   :draw draw
   :key-typed my-key-press
   :mouse-pressed bed/mouse-pressed
   :mouse-released bed/mouse-released
   :mouse-dragged bed/mouse-dragged
   :mouse-wheel bed/mouse-wheel
   :size [600 500]
   :features [:resizable]
   :middleware [quil.middleware/fun-mode]))
