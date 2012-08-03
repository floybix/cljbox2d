(ns cljbox2d.testbed
  (:use [cljbox2d core joints])
  (:require [quil.core :as quil])
  (:require [quil.helpers.drawing :as quild]))

(def ^:dynamic *world-view*
  {:width 60 :height 40 :x-middle 0 :y-bottom -2})

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

(defn draw-world
  "Draw all shapes (fixtures) from the Box2D world"
  []
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
      :otherwise-ignore-it
      ))
  (doseq [fx (fixtureseq)
          :let [typ (shape-type fx)
                pts (world-coords fx)
                px-pts (map world-to-pixels pts)
                [x0 y0] (first px-pts)
                radius-px (world-dist-to-pixels (radius fx))]]
    (case typ
      :circle (quil/ellipse x0 y0 radius-px radius-px)
      :polygon (doseq [coords (quild/line-join-points px-pts)]
                 (apply quil/line coords)))))

(defn setup-style []
  (quil/frame-rate 30)
  (quil/stroke 128)
  (quil/stroke-weight 1)
  (quil/fill 255 64)
  (quil/background 0))
