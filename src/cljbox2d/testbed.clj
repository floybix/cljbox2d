(ns cljbox2d.testbed
  (:use [cljbox2d core joints])
  (:require [quil.core :as quil])
  (:require [quil.helpers.drawing :as quild]))

(def info-text (atom ""))

(def camera (atom {:width 60 :height 40 :x-left -30 :y-bottom -10}))

(defn world-to-px-scale
  "A scaling factor on world coordinates to give pixels.
Fits the camera bounds into the window, expanding these
bounds if necessary to ensure an isometric aspect ratio."
  ([]
     (world-to-px-scale (quil/width) (quil/height)))
  ([px-width px-height]
     (let [cam @camera
           xscale (/ px-width (:width cam))
           yscale (/ px-height (:height cam))]
       (min xscale yscale))))

(defn world-to-px
  "Convert a point in Box2d world coordinates to quil pixels."
  ([[x y]]
     (let [cam @camera
           scale (world-to-px-scale)
           x-left (:x-left cam)
           y-bottom (:y-bottom cam)
           y-top (+ y-bottom (:height cam))]
       [(* (- x x-left) scale)
        ;; quil has flipped y (0px at top)
        (* (- y-top y) scale)])))

(defn px-to-world
  "Convert a point in quil pixels to Box2d world coordinates."
  ([[xp yp]]
     (let [cam @camera
           scale (world-to-px-scale)
           x-left (:x-left cam)
           y-bottom (:y-bottom cam)
           y-top (+ y-bottom (:height cam))]
       [(+ (/ xp scale) x-left)
        ;; quil has flipped y (0px at top)
        (- y-top (/ yp scale))])))

(defn setup-style []
  (quil/background 0)
  (quil/stroke-weight 1))

(defn body-style []
  (let [clr (quil/color 255 200 200)]
    (quil/stroke clr)
    (quil/fill clr 127)))

(defn joint-style []
  (let [blue (quil/color 155 155 255)]
    (quil/stroke blue)
    (quil/fill blue 127)))

(defn static-style []
  (let [green (quil/color 100 255 100)]
    (quil/stroke green)
    (quil/fill green 127)))

(defn draw-world
  "Draw all shapes (fixtures) from the Box2D world"
  []
  (setup-style)
  (joint-style)
  (doseq [jt (jointseq)
          :let [typ (joint-type jt)
                body-a (.getBodyA jt)
                body-b (.getBodyB jt)]]
    (case typ
      :revolute (let [anch (anchor-a jt)
                      center-a (world-center body-a)
                      center-b (world-center body-b)]
                  (quil/line (world-to-px anch) (world-to-px center-a))
                  (quil/line (world-to-px anch) (world-to-px center-b)))
      :distance (let [anch-a (anchor-a jt)
                      anch-b (anchor-b jt)]
                  (quil/line (world-to-px anch-a) (world-to-px anch-b)))
      :otherwise-ignore-it
      ))
  (doseq [fx (fixtureseq)
          :let [body (body fx)
                body-typ (body-type body)
                shp-typ (shape-type fx)
                pts (world-coords fx)
                px-pts (map world-to-px pts)
                [x0 y0] (first px-pts)
                radius-px (* (radius fx) (world-to-px-scale))]]
    (case body-typ
      :static (static-style)
      :dynamic (body-style))
    (case shp-typ
      :circle (quil/ellipse x0 y0 radius-px radius-px)
      :polygon (do
                 (quil/begin-shape)
                 (doseq [[x y] px-pts] (quil/vertex x y))
                 (quil/end-shape :close))))
  (quil/fill 255)
  (quil/text @info-text 10 10))

;; event handling

(defn right-mouse-dragged []
  (let [[x y] (px-to-world [(quil/mouse-x) (quil/mouse-y)])
        [ox oy] (px-to-world [(quil/pmouse-x) (quil/pmouse-y)])
        dx (- x ox)
        dy (- y oy)]
    (swap! camera
           (fn [old] (merge old {:x-left (+ (:x-left old) (- dx))
                                 :y-bottom (+ (:y-bottom old) (- dy))})))))

(defn left-mouse-dragged [] )

(defn mouse-dragged []
  (case (quil/mouse-button)
    :right (right-mouse-dragged)
    :left (left-mouse-dragged)
    :otherwise-ignore-it))

