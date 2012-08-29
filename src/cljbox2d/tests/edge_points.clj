(ns cljbox2d.tests.edge-points
  "An example of cljbox2d.core/edge-point"
  (:use (cljbox2d core joints testbed)
        [cljbox2d.vec2d :only [TWOPI PI poly-flip-x angle*]])
  (:require [quil.core :as quil]))

(defn setup-world! []
  (create-world!)
  (let [ground (body! {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        angles (range 0 TWOPI (/ TWOPI 16))
        shell-v (list [0 0] [8 0] [8 0.5]
                      [6 1.5] [3 2.5]
                      [1 2] [0.5 1.5])
        ;; make a beetle body with radial fixtures.
        ;; get edge-points just on the hub fixture
        ;; (because we are adding fixtures to the body)
        hub (body! {:position [0 10]}
                   {:shape (polygon shell-v)})
        fx (fixture hub)
        spokes (doall (for [angle angles
                            :let [pt (edge-point fx angle)
                                  locpt (to-local hub pt)]]
                        (fixture! hub
                                  {:shape (rod locpt angle 1 0.2)})))
        eye (let [pt (edge-point fx (angle* :right) 0.9)
                  locpt (to-local hub pt)]
              (fixture! hub {:shape (circle 0.7 locpt)}))
        ;; another beetle body, with radial jointed bodies.
        ;; this time we get edge-points on the whole body (2 fixtures)
        hub2 (body! {:position [20 10]}
                    {:shape (polygon (poly-flip-x shell-v))})
        eye2 (let [pt (edge-point hub2 (angle* :left) 1.1)
                   locpt (to-local hub2 pt)]
               (fixture! hub2 {:shape (circle 0.7 locpt)}))
        spokes2 (doall (for [angle angles
                             :let [pt (edge-point hub2 angle)]]
                         (let [b (body! {:position pt}
                                        {:shape (rod [0 0] angle 1 0.2)
                                         :group-index -2})]
                           (revolute-joint! hub2 b pt
                                            {:enable-motor true
                                             :motor-speed (/ PI 2)
                                             :max-motor-torque 100}))))]
    (reset! ground-body ground)))

(defn setup []
  (setup-world!))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Edge points"
    :setup setup
    :draw draw
    :key-typed key-press
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :size [600 500]))
