(ns org.nfrac.liquidfun.testbed.tests.web
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.Web"
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf
             :refer [body! joint!]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(defn setup []
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-40 0] [40 0])})
        nodeshape (lf/box 0.5 0.5)
        nodes (for [pt [[-5 5] [5 5] [5 15] [-5 15]]]
                (body! world {:position pt}
                       {:shape nodeshape :density 5}))
        x-middle 0
        y-middle 10
        ground-joints (for [nd nodes
                            :let [[x y] (lf/position nd)
                                  is-right (> x x-middle)
                                  is-top (> y y-middle)
                                  ground-x (if is-right 10 -10)
                                  ground-y (if is-top 20 0)
                                  off-x (if is-right 0.5 -0.5)
                                  off-y (if is-top 0.5 -0.5)]]
                        (joint! {:type :distance
                                 :body-a ground
                                 :body-b nd
                                 :world-anchor-a [ground-x ground-y]
                                 :world-anchor-b [(+ x off-x) (+ y off-y)]
                                 :frequency-hz 4
                                 :damping-ratio 0.5}))
        inner-joints (for [i (range 4)
                           :let [n1 (nth nodes i)
                                 n2 (nth nodes (mod (inc i) 4))
                                 [x1 y1] (lf/position n1)
                                 [x2 y2] (lf/position n2)
                                 off-x1 (* -0.5 (compare x1 x2))
                                 off-y1 (* -0.5 (compare y1 y2))]]
                       (joint! {:type :distance
                                :body-a n1
                                :body-b n2
                                :world-anchor-a [(+ x1 off-x1) (+ y1 off-y1)]
                                :world-anchor-b [(- x2 off-x1) (- y2 off-y1)]
                                :frequency-hz 4
                                :damping-ratio 0.5}))]
    (doall ground-joints)
    (doall inner-joints)
    (assoc bed/initial-state
      :world world)))

(defn my-key-press
  [state event]
  (let [world (:world state)]
    (case (:key event)
      :b (do
           (when-let [bod (first (lf/bodyseq world))]
             (lf/destroy-body! bod))
           state)
      :j (do
           (when-let [jt (first (lf/alljointseq world))]
             (lf/destroy-joint! jt))
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
  (quil/text (str "This demonstrates a soft distance joint." "\n"
                  "Press: (b) to delete a body, (j) to delete a joint")
             10 10))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Web"
   :host "liquidfun"
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
