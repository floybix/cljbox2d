(ns org.nfrac.cljbox2d.testbed.tests.edge-points
  "An example of cljbox2d.core/edge-point"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d
             :refer [TWOPI PI PI_2 poly-flip-x angle-up? abs
                     poly-edge-point v-interp]]
            [quil.core :as quil]
            [quil.middleware]))

(def LEFT PI)
(def RIGHT 0)
(def DOWN (- PI_2))

(defn edge-point
  "World coordinates on the edge of a fixture in a given direction
   from its center. `frac` gives the fraction out towards the edge (or
   outside if > 1)."
  [fixt angle frac]
  (let [origin-pt (center fixt)
        edge-pt (poly-edge-point (world-coords fixt) angle origin-pt)]
    (if (nil? edge-pt) nil
        (v-interp origin-pt edge-pt frac))))

(defn setup []
  (quil/frame-rate 60)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        angles (range 0 TWOPI (/ TWOPI 24))
        shell-v (list [0 0] [4 0] [4 0.1]
                      [3 0.75] [1.5 1.25]
                      [0.5 1.0] [0.25 0.75])
        ;; make a beetle body with fixtures attached around edges.
        hub (body! world {:position [0 10]}
                   {:shape (polygon shell-v)})
        fx (fixture-of hub)
        spokes (doall (for [angle angles
                            :let [pt (edge-point fx angle 1.0)
                                  locpt (to-local hub pt)
                                  ext-angle (if (or (zero? angle)
                                                    (angle-up? angle))
                                              angle
                                              DOWN)]]
                        (fixture! hub
                                  {:shape (rod locpt ext-angle 0.2 0.05)})))
        eye (let [pt (edge-point fx RIGHT 0.8)
                  locpt (to-local hub pt)]
              (fixture! hub {:shape (circle 0.3 locpt)}))
        ;; another beetle body, with jointed bodies attached around edges.
        hub2 (body! world {:position [10 10]}
                    {:shape (polygon (poly-flip-x shell-v))})
        fx2 (fixture-of hub2)
        eye2 (let [pt (edge-point fx2 LEFT 0.8)
                   locpt (to-local hub2 pt)]
               (fixture! hub2 {:shape (circle 0.3 locpt)}))
        spokes2 (doall (for [angle angles
                             :let [pt (edge-point fx2 angle 1.0)]]
                         (let [b (body! world {:position pt}
                                        {:shape (rod [0 0] angle 0.4 0.1)
                                         :group-index -2})]
                           (joint! {:type :revolute
                                    :body-a hub2
                                    :body-b b
                                    :world-anchor pt
                                    :enable-motor true
                                    :motor-speed PI
                                    :max-motor-torque 100}))))]
    (assoc bed/initial-state
      :dt-secs (/ 1 60.0)
      :world world)))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Edge points"
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
