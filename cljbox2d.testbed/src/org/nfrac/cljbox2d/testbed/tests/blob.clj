(ns org.nfrac.cljbox2d.testbed.tests.blob
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.BlobTest4"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d :refer [TWOPI polar-xy]]
            [quil.core :as quil]
            [quil.middleware]))

(def NBODY 20)

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (box 50 0.4)}
                      {:shape (box 0.4 50 [-10 0])}
                      {:shape (box 0.4 50 [10 0])})
        [cx cy] [0 10] ;; center
        radius 5
        angles (range 0 TWOPI (/ TWOPI NBODY))
        nodes (for [angle angles
                    :let [[x y] (polar-xy radius angle)]]
                (body! world {:position [(+ x cx) (+ y cy)]
                              :fixed-rotation true}
                       {:shape (circle 0.5)
                        :group-index -2}))
        cvj (joint! {:type :constant-volume
                     :bodies nodes
                     :frequency-hz 10
                     :damping-ratio 1})
        falling-box (body! world {:position [cx (+ cy 15)]}
                           {:shape (box 3 1.5 [cx (+ cy 15)])})]
    (assoc bed/initial-state
      :world world
      ::things {:nodes nodes :cvj cvj})))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Blob Joint"
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
