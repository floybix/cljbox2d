(ns cljbox2d.testbed
  (:use [cljbox2d core joints])
  (:require [quil.core :as quil])
  (:require [quil.helpers.drawing :as quild]))

(def ^:dynamic *world-view*
  {:width 60 :height 40 :x-middle 0 :y-bottom -10})

(def info-text (atom ""))

(defn world-to-pixels
  "Convert a point in Box2d world coordinates to quil pixels.
Fits the *world-view* bounds into the window, expanding the
bounds if necessary to ensure an isometric aspect ratio."
  ([pt]
     (world-to-pixels pt (quil/width) (quil/height)))
  ([[x y] px-width px-height]
     (let [wv *world-view*
           xscale (/ px-width (:width wv))
           yscale (/ px-height (:height wv))
           scale (min xscale yscale)
           x-left (- (:x-middle wv) (/ (:width wv) 2))
           y-bottom (:y-bottom wv)]
       [(* (- x x-left) scale)
        (- px-height ;; quil is flipped y
           (* (- y y-bottom) scale))])))

(defn world-dist-to-pixels
  "Convert a distance in Box2d world coordinates to quil pixels."
  ([d]
     (- (first (world-to-pixels [d 0]))
        (first (world-to-pixels [0 0])))))

(defn setup-style []
  (quil/background 0)
  (quil/stroke-weight 1))

(defn body-style []
  (let [clr (quil/color 255 200 200)]
    (quil/stroke clr)
    (quil/fill clr 127)))

(defn joint-style []
  (let [blue (quil/color 150 150 240)]
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
                  (quil/line (world-to-pixels anch) (world-to-pixels center-a))
                  (quil/line (world-to-pixels anch) (world-to-pixels center-b)))
      :distance (let [anch-a (anchor-a jt)
                      anch-b (anchor-b jt)]
                  (quil/line (world-to-pixels anch-a) (world-to-pixels anch-b)))
      :otherwise-ignore-it
      ))
  (doseq [fx (fixtureseq)
          :let [body (body fx)
                body-typ (body-type body)
                shp-typ (shape-type fx)
                pts (world-coords fx)
                px-pts (map world-to-pixels pts)
                [x0 y0] (first px-pts)
                radius-px (world-dist-to-pixels (radius fx))]]
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
