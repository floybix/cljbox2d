(ns org.nfrac.cljbox2d.testbed.tests.web
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.Web"
  (:require [org.nfrac.cljbox2d.testbed :as bed :refer [the-world
                                                        dt-secs]]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [quil.core :as quil]))

(def things (atom {}))

(defn setup-world! []
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        nodeshape (box 0.5 0.5)
        nodes (for [pt [[-5 5] [5 5] [5 15] [-5 15]]]
                (body! world {:position pt}
                       {:shape nodeshape :density 5}))
        x-middle 0
        y-middle 10
        ground-joints (for [nd nodes
                            :let [[x y] (position nd)
                                  is-right (> x x-middle)
                                  is-top (> y y-middle)
                                  ground-x (if is-right 10 -10)
                                  ground-y (if is-top 20 0)
                                  off-x (if is-right 0.5 -0.5)
                                  off-y (if is-top 0.5 -0.5)]]
                        (distance-joint! world ground nd
                                         [ground-x ground-y]
                                         [(+ x off-x) (+ y off-y)]
                                         {:frequency-hz 4
                                          :damping-ratio 0.5}))
        inner-joints (for [i (range 4)
                           :let [n1 (nth nodes i)
                                 n2 (nth nodes (mod (inc i) 4))
                                 [x1 y1] (position n1)
                                 [x2 y2] (position n2)
                                 off-x1 (* -0.5 (compare x1 x2))
                                 off-y1 (* -0.5 (compare y1 y2))]]
                       (distance-joint! world n1 n2
                                        [(+ x1 off-x1) (+ y1 off-y1)]
                                        [(- x2 off-x1) (- y2 off-y1)]
                                        {:frequency-hz 4
                                         :damping-ratio 0.5}))]
    (reset! the-world world)
    (reset! things {:nodes (doall nodes)
                    :joints (doall (concat ground-joints inner-joints))})))

(defn my-key-press []
  (let [jts (:joints @things)
        nodes (:nodes @things)]
    (case (quil/raw-key)
      \b (when-let [bod (first nodes)]
           (swap! things update-in [:nodes] next)
           (destroy! bod))
      \j (when-let [jt (first jts)]
           (swap! things update-in [:joints] next)
           (destroy! jt))
      ;; otherwise pass on to testbed
      (bed/key-press))))

(defn setup []
  (quil/frame-rate (/ 1 @dt-secs))
  (setup-world!))

(defn draw []
  (when-not @bed/paused?
    (step! @the-world @dt-secs))
  (bed/draw-world @the-world)
  (quil/fill 255)
  (quil/text (str "This demonstrates a soft distance joint." "\n"
                  "Press: (b) to delete a body, (j) to delete a joint")
             10 10))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Web"
    :setup setup
    :draw draw
    :key-typed my-key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]))
