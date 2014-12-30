(ns org.nfrac.cljbox2d.testbed.tests.blob
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.BlobTest4"
  (:require [org.nfrac.cljbox2d.testbed :as bed :refer [the-world
                                                        dt-secs]]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer [constant-volume-joint!]]
            [cljbox2d.vec2d :refer [TWOPI polar-xy]]
            [quil.core :as quil]))

(def things (atom {}))

(def NBODY 20)

(defn setup-world! []
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (box 50 0.4)}
                      {:shape (box 0.4 50 [-10 0])}
                      {:shape (box 0.4 50 [10 0])})
        [cx cy] [0 10], rad 5 ;; center, radius
        angles (range 0 TWOPI (/ TWOPI NBODY))
        nodes (for [angle angles
                    :let [[x y] (polar-xy rad angle)]]
                (body! world {:position [(+ x cx) (+ y cy)]
                              :fixed-rotation true}
                       {:shape (circle 0.5)
                        :group-index -2}))
        cvj (constant-volume-joint! world nodes
                                    {:frequency-hz 10
                                     :damping-ratio 1})
        falling-box (body! world {:position [cx (+ cy 15)]}
                           {:shape (box 3 1.5 [cx (+ cy 15)])})]
    (reset! the-world world)
    (reset! things {:nodes nodes :cvj cvj})))

(defn setup []
  (quil/frame-rate (/ 1 @dt-secs))
  (setup-world!))

(defn draw []
  (when-not @bed/paused?
    (step! @the-world @dt-secs))
  (bed/draw-world @the-world))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Blob Joint"
    :setup setup
    :draw draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]))
