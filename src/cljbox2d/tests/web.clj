(ns cljbox2d.tests.web
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.Web"
  (:use [cljbox2d core joints testbed])
  (:require [quil.core :as quil]))

(def things (atom {}))

(defn setup-world! []
  (create-world!)
  (let [ground (body! {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        nodeshape (box 0.5 0.5)
        nodes (for [pt [[-5 5] [5 5] [5 15] [-5 15]]]
                (body! {:position pt}
                       {:shape nodeshape :density 5}))
        x-middle 0
        y-middle 10
        ground-joints (for [nd nodes
                            :let [[x y] (world-point nd)
                                  is-right (> x x-middle)
                                  is-top (> y y-middle)
                                  ground-x (if is-right 10 -10)
                                  ground-y (if is-top 20 0)
                                  off-x (if is-right 0.5 -0.5)
                                  off-y (if is-top 0.5 -0.5)]]
                        (distance-joint! ground nd
                                         [ground-x ground-y]
                                         [(+ x off-x) (+ y off-y)]
                                         {:frequency-hz 4
                                          :damping-ratio 0.5}))
        inner-joints (for [i (range 4)
                           :let [n1 (nth nodes i)
                                 n2 (nth nodes (mod (inc i) 4))
                                 [x1 y1] (world-point n1)
                                 [x2 y2] (world-point n2)
                                 off-x1 (* -0.5 (compare x1 x2))
                                 off-y1 (* -0.5 (compare y1 y2))]]
                       (distance-joint! n1 n2
                                        [(+ x1 off-x1) (+ y1 off-y1)]
                                        [(- x2 off-x1) (- y2 off-y1)]
                                        {:frequency-hz 4
                                         :damping-ratio 0.5}))]
    (reset! things {:nodes (doall nodes)
                    :joints (doall (concat ground-joints inner-joints))})
    (reset! ground-body ground)))

(defn update-info-text []
  (reset! info-text
          (str "This demonstrates a soft distance joint." "\n"
               "Press: (b) to delete a body, (j) to delete a joint")))

(defn key-press []
  (let [jts (:joints @things)
        nodes (:nodes @things)]
    (case (quil/raw-key)
      \b (when-let [bod (first nodes)]
           (swap! things update-in [:nodes] next)
           (destroy! bod))
      \j (when-let [jt (first jts)]
           (swap! things update-in [:joints] next)
           (destroy! jt))
      :otherwise-ignore-it))
  (update-info-text))

(defn setup []
  (setup-world!)
  (update-info-text))

(defn draw []
  (step! (/ 1 (quil/current-frame-rate)))
  (draw-world))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Web"
    :setup setup
    :draw draw
    :key-typed key-press
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :size [600 500]))
