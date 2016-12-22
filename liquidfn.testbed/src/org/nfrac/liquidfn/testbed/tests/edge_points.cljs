(ns org.nfrac.liquidfn.testbed.tests.edge-points
  "An example of liquidfn.vec2/poly-edge-point"
  (:require [org.nfrac.liquidfn.testbed :as bed]
            [org.nfrac.liquidfn.core :as lf :refer [body! joint! fixture!]]
            [org.nfrac.liquidfn.vec2d
             :refer [TWOPI PI PI_2 poly-flip-x angle-up? abs
                     poly-edge-point v-interp]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(def LEFT PI)
(def RIGHT 0)
(def DOWN (- PI_2))

(defn bounding-box
  [coords]
  (let [xs (map first coords)
        ys (map second coords)]
    [[(reduce min xs) (reduce min ys)]
     [(reduce max xs) (reduce max ys)]]))

(defn edge-point
  "World coordinates on the edge of a fixture in a given direction
   from its center. `frac` gives the fraction out towards the edge (or
   outside if > 1)."
  [fixt angle frac]
  (let [coords (lf/world-coords fixt)
        [[x0 y0] [x1 y1]] (bounding-box coords)
        origin-pt [(* 0.5 (+ x0 x1)) (* 0.5 (+ y0 y1))]
        edge-pt (poly-edge-point coords angle origin-pt)]
    (if (nil? edge-pt) nil
        (v-interp origin-pt edge-pt frac))))

(defn setup []
  (quil/frame-rate 60)
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-40 0] [40 0])})
        angles (range 0 TWOPI (/ TWOPI 24))
        shell-v (list [0 0] [4 0] [4 0.1]
                      [3 0.75] [1.5 1.25]
                      [0.5 1.0] [0.25 0.75])
        ;; make a beetle body with fixtures attached around edges.
        hub (body! world {:position [0 10]}
                   {:shape (lf/polygon shell-v)})
        fx (lf/fixture-of hub)
        spokes (doall (for [angle angles
                            :let [pt (edge-point fx angle 1.0)
                                  locpt (lf/to-local hub pt)
                                  ext-angle (if (or (zero? angle)
                                                    (angle-up? angle))
                                              angle
                                              DOWN)]]
                        (fixture! hub
                                  {:shape (lf/rod locpt ext-angle 0.2 0.05)})))
        eye (let [pt (edge-point fx RIGHT 0.8)
                  locpt (lf/to-local hub pt)]
              (fixture! hub {:shape (lf/circle 0.3 locpt)}))
        ;; another beetle body, with jointed bodies attached around edges.
        hub2 (body! world {:position [10 10]}
                    {:shape (lf/polygon (poly-flip-x shell-v))})
        fx2 (lf/fixture-of hub2)
        eye2 (let [pt (edge-point fx2 LEFT 0.8)
                   locpt (lf/to-local hub2 pt)]
               (fixture! hub2 {:shape (lf/circle 0.3 locpt)}))
        spokes2 (doall (for [angle angles
                             :let [pt (edge-point fx2 angle 1.0)]]
                         (let [b (body! world {:position pt}
                                        {:shape (lf/rod [0 0] angle 0.4 0.1)
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

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Edge points"
   :host "liquidfn"
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
