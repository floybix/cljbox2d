(ns org.nfrac.liquidfn.testbed.tests.rope-joint
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.RopeTest"
  (:require [org.nfrac.liquidfn.testbed :as bed]
            [org.nfrac.liquidfn.core :as lf :refer [body! joint!]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(defn setup []
  (quil/frame-rate 30)
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-40 0] [40 0])})
        link-fd {:shape (lf/box 0.6 0.125)
                 :group-index -1 ;; never collide
                 :density 20
                 :friction 0.2}
        block-fd {:shape (lf/box 1.5 1.5)
                  :group-index -1 ;; never collide
                  :density 100
                  :friction 0.2}
        ;; TODO categoryBits? maskBits?
        N 10
        y 15.0
        linktip (reduce (fn [prev-body i]
                          (let [end? (== i (dec N))
                                b (body! world {:position [(+ 0.5 i) y]
                                                :angular-damping (if end? 0.4 0.0)}
                                         (if end? block-fd link-fd))]
                            (joint! {:type :revolute
                                     :body-a prev-body
                                     :body-b b
                                     :world-anchor [i y]
                                     :collide-connected false})
                            b))
                        ground
                        (range N))
        rope-spec {:type :rope
                   :body-a ground
                   :body-b linktip
                   :anchor-a [0 y]
                   :anchor-b [0 0]
                   :max-length (+ (- N 1) 0.01)}
        rope-j (joint! rope-spec)]
    (assoc bed/initial-state
      :world world
      ::rope-spec rope-spec
      ::rope-j rope-j)))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn draw
  [state]
  (bed/draw state)
  (let [jt (::rope-j state)]
    (quil/fill 255)
    (quil/text (str "Press (j) to toggle the rope joint." "\n"
                    (if jt "Rope ON" "Rope OFF"))
               10 10)))

(defn my-key-press
  [state event]
  (let [rope-j (::rope-j state)
        rope-spec (::rope-spec state)]
    (case (:key event)
      :j (if rope-j
           (do
             (lf/destroy-joint! rope-j)
             (assoc state ::rope-j nil))
           (do
             (assoc state ::rope-j (joint! rope-spec))))
      ;; otherwise pass on to testbed
      (bed/key-press state event))))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Rope Joint"
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
