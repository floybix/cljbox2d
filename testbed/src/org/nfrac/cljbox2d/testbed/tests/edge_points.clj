(ns org.nfrac.cljbox2d.testbed.tests.edge-points
  "An example of cljbox2d.core/edge-point"
  (:require [org.nfrac.cljbox2d.testbed :as bed :refer [*timestep*]]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [cljbox2d.vec2d :refer [TWOPI PI poly-flip-x angle*]]
            [quil.core :as quil]))

(defn setup-world! []
  (reset-world! (new-world))
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
    (reset! bed/ground-body ground)))

(defn setup []
  (quil/frame-rate (/ 1 *timestep*))
  (setup-world!))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Edge points"
    :setup setup
    :draw bed/draw
    :key-typed bed/key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]))
