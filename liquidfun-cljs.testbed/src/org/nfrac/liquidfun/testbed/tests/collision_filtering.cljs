(ns org.nfrac.liquidfun.testbed.tests.collision-filtering
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.CollisionFiltering"
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf :refer [body! joint!]]
            [quil.core :as quil :include-macros true]
            [quil.middleware]))

(defn scale-vv
  "Scale a vector of vertices around [0 0]."
  [vv s]
  (map (fn [[x y]] [(* x s) (* y s)]) vv))

(defn setup []
  (quil/frame-rate 30)
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-40 0] [40 0])
                       :friction 0.3})
        small-group 1 ;; positive = collide
        large-group -1 ;; negative = not collide
        cats {:default 0x0001
              :triangle 0x0002
              :box 0x0004
              :circle 0x0008}
        masks {:triangle 0xFFFF
               :box (bit-xor 0xFFFF (:triangle cats))
               :circle 0xFFFF}
        ;; left group (triangles)
        tri-vv [[-1 0] [1 0] [0 2]]
        tri-attr {:density 1.0
                  :category-bits (:triangle cats)
                  :mask-bits (:triangle masks)}
        tri-small (body! world {:position [-5 2]}
                         (assoc tri-attr
                           :shape (lf/polygon tri-vv)
                           :group-index small-group))
        tri-big (body! world {:position [-5 6]
                              :fixed-rotation true}
                       (assoc tri-attr
                         :shape (lf/polygon (scale-vv tri-vv 2))
                         :group-index large-group))
        tri-box (body! world {:position [-5 10]}
                       {:shape (lf/box 0.5 1)
                        :density 1.0})
        tri-j (joint! {:type :prismatic
                       :body-a tri-big
                       :body-b tri-box
                       :anchor-a [0 4]
                       :anchor-b [0 0]
                       :axis-a [0 1]
                       :enable-limit true
                       :lower-trans -1
                       :upper-trans 1})
        ;; centre group (boxes)
        box-attr {:density 1.0
                  :restitution 0.1
                  :category-bits (:box cats)
                  :mask-bits (:box masks)}
        box-small (body! world {:position [0 2]}
                         (assoc box-attr
                           :shape (lf/box 1 0.5)
                           :group-index small-group))
        box-big (body! world {:position [0 6]}
                       (assoc box-attr
                         :shape (lf/box 2 1)
                         :group-index large-group))
        ;; right group (circles)
        circ-attr {:density 1.0
                   :category-bits (:circle cats)
                   :mask-bits (:circle masks)}
        circ-small (body! world {:position [5 2]}
                          (assoc circ-attr
                            :shape (lf/circle 1)
                            :group-index small-group))
        circ-big (body! world {:position [5 6]}
                       (assoc circ-attr
                         :shape (lf/circle 2)
                         :group-index large-group))]
    (assoc bed/initial-state
      :world world)))

(defn step
  [state]
  (-> (bed/world-step state)
      (bed/record-snapshot true)))

(defn draw
  [state]
  (bed/draw state)
  (quil/fill 255)
  (quil/text (str "Collision Filtering.\n"
                  "The 3 small shapes always collide.\n"
                  "The 3 large shapes never collide.\n"
                  "The boxes don't collide with triangles (except if both are small).")
             10 10))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Collision Processing"
   :host "liquidfun"
   :setup setup
   :update (fn [s] (if (:paused? s) s (step s)))
   :draw draw
   :key-typed bed/key-press
   :mouse-pressed bed/mouse-pressed
   :mouse-released bed/mouse-released
   :mouse-dragged bed/mouse-dragged
   :mouse-wheel bed/mouse-wheel
   :size [600 500]
   :features [:resizable]
   :middleware [quil.middleware/fun-mode]))
