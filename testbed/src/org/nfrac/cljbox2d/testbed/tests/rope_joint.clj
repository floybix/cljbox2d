(ns org.nfrac.cljbox2d.testbed.tests.rope-joint
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.RopeTest"
  (:require [org.nfrac.cljbox2d.testbed :as bed]
            [cljbox2d.core :refer :all]
            [cljbox2d.joints :refer :all]
            [quil.core :as quil]
            [quil.middleware]))

(defn setup []
  (quil/frame-rate 30)
  (let [world (new-world)
        ground (body! world {:type :static}
                      {:shape (edge [-40 0] [40 0])})
        link-fd {:shape (box 0.6 0.125)
                 :group-index -1 ;; never collide
                 :density 20
                 :friction 0.2}
        block-fd {:shape (box 1.5 1.5)
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
                                     :anchor [i y]
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
  (if (:paused? state)
    state
    (update-in state [:world] step! (:dt-secs state))))

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
    (case (:raw-key event)
      \j (if rope-j
           (do
             (destroy! rope-j)
             (assoc state ::rope-j nil))
           (do
             (assoc state ::rope-j (joint! rope-spec))))
      ;; otherwise pass on to testbed
      (bed/key-press state event))))

(defn -main
  "Run the test sketch."
  [& args]
  (quil/defsketch test-sketch
    :title "Rope Joint"
    :setup setup
    :update step
    :draw draw
    :key-typed my-key-press
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :size [600 500]
    :middleware [quil.middleware/fun-mode]))
