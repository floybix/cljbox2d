(ns cljbox2d.testbed
  "Provides the scaffolding to run Box2D simulations with visual
  animation and interactive inputs (via mouse and keyboard). This is
  built on the Processing framework through the clojure wrapper
  [quil](https://github.com/quil/quil). Box2D timesteps are run
  synchronously with Processing (draw) timesteps: in this way we take
  advantage of Processing's queuing of inputs to avoid concurrency
  issues. In a real application you might want to run the drawing and
  physics in separate threads.

  In this namespace we have drawing functions, some vars/atoms for
  hooking in to the testbed, a default contact listener, and default
  input event handlers."
  (:use [cljbox2d.core :only [*world* world-time step! bodyseq
                              fixtureseq body-type shape-type body
                              center world-coords radius mass
                              query-at-point destroy!
                              awake? wake! v2xy vec2]]
        [cljbox2d.joints :only [jointseq joint-type
                                body-a body-b anchor-a anchor-b
                                mouse-joint!]])
  (:import (org.jbox2d.callbacks ContactListener)
           (org.jbox2d.collision WorldManifold))
  (:require [quil.core :as quil]))

(def ^:dynamic *timestep* (/ 1 30.0))

;; ## Atoms acting as hooks

(def info-text
  "Text to draw in the corner of the sketch"
  (atom ""))

(def camera
  "Defines the current view (location and scale) in world coordinates"
  (atom {:width 60 :height 40 :x-left -30 :y-bottom -10}))

(def mousej
  "Current mouse joint; see `left-mouse-pressed` etc."
  (atom nil))

(def paused "Whether to simulate or not." (atom false))

(def step-fn
  "A function to run every simulated time step, e.g. for contact
   processing or to apply special forces."
  (atom (fn [])))

(def draw-more-fn
  "A function to run every frame after drawing the world objects."
  (atom (fn [])))

(def ground-body
  "A static body, used as a reference for e.g. mouse joints"
  (atom nil))

;; ## Drawing

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

(defn setup-style
  "Set common drawing style attributes"
  []
  (quil/background 0)
  (quil/stroke-weight 1))

(defn dynamic-style
  "Set drawing style for awake dynamic bodies."
  []
  (let [clr (quil/color 255 200 200)]
    (quil/stroke clr)
    (quil/fill clr 127)))

(defn sleeping-style
  "Set drawing style for sleeping dynamic bodies."
  []
  (let [clr (quil/color 150 150 150)]
    (quil/stroke clr)
    (quil/fill clr 127)))

(defn joint-style
  "Set drawing style for joints"
  []
  (let [blue (quil/color 155 155 255)]
    (quil/stroke blue)
    (quil/fill blue 127)))

(defn static-style
  "Set drawing style for static bodies."
  []
  (let [green (quil/color 100 255 100)]
    (quil/stroke green)
    (quil/fill green 127)))

(defn draw-world
  "Draw all shapes (fixtures) and joints in the Box2D world.
   Also draws @info-text."
  []
  (setup-style)
  (joint-style)
  (doseq [jt (jointseq)
          :let [typ (joint-type jt)
                body-a (body-a jt)
                body-b (body-b jt)]]
    (case typ
      :revolute (let [anch (anchor-a jt)
                      center-a (center body-a)
                      center-b (center body-b)]
                  (quil/line (world-to-px anch) (world-to-px center-a))
                  (quil/line (world-to-px anch) (world-to-px center-b)))
      :distance (let [anch-a (anchor-a jt)
                      anch-b (anchor-b jt)]
                  (quil/line (world-to-px anch-a) (world-to-px anch-b)))
      :mouse (let [anch-b (anchor-b jt)
                   targ (v2xy (.getTarget jt))]
               (quil/line (world-to-px anch-b) (world-to-px targ)))
      :otherwise-ignore-it
      ))
  (doseq [body (bodyseq)]
    (case (body-type body)
      :static (static-style)
      :dynamic (if (awake? body) (dynamic-style) (sleeping-style)))
    (doseq [fx (fixtureseq body)]
      (case (shape-type fx)
        :circle (let [[x y] (world-to-px (center fx))
                      radius-px (* (radius fx) (world-to-px-scale))]
                  (quil/ellipse x y (* 2 radius-px) (* 2 radius-px)))
        :polygon (let [pts (world-coords fx)
                       px-pts (map world-to-px pts)]
                   (quil/begin-shape)
                   (doseq [[x y] px-pts] (quil/vertex x y))
                   (quil/end-shape :close)))))
  (quil/fill 255)
  (quil/text @info-text 10 10))

(defn draw
  "Draw handler for quil. Calls `draw-world`.
   After drawing calls the hook function `@draw-more-fn`.
   After simulation calls the hook function `@step-fn`"
  []
  (when-not @paused
    (step! *timestep*)
    (@step-fn))
  (draw-world)
  (@draw-more-fn))

;; ## contact / collision handling

(def contact-buffer
  "Holds a sequence of contacts for the last time step, each
represented as `[fixture-a fixture-b points normal]`."
  (atom []))

(defn set-buffering-contact-listener!
  "A ContactListener which populates `contact-buffer`."
  []
  (let [world-manifold (WorldManifold.)
        lstnr (reify ContactListener
                (beginContact [_ _])
                (endContact [_ _])
                (postSolve [_ _ _])
                (preSolve [_ contact _]
                  (let [manifold (.getManifold contact)
                        pcount (.pointCount manifold)]
                    (when (pos? pcount)
                      ;; mutates its argument:
                      (.getWorldManifold contact world-manifold)
                      (let [fixt-a (.getFixtureA contact)
                            fixt-b (.getFixtureB contact)
                            -points (.points world-manifold)
                            pts (map v2xy (take pcount -points))
                            normal (v2xy (.normal world-manifold))]
                        (swap! contact-buffer conj
                               [fixt-a fixt-b pts normal])
                        )))))]
    (.setContactListener *world* lstnr)))
    
;; ## input event handling

(defn mouse-world
  "Current mouse position in world coordinates."
  []
  (px-to-world [(quil/mouse-x) (quil/mouse-y)]))

(defn pmouse-world
  "Previous time-step mouse position in world coordinates."
  []
  (px-to-world [(quil/pmouse-x) (quil/pmouse-y)]))

(defn left-mouse-pressed
  "Checks for fixtures at the mouse position. If one is found, creates
a mouse joint attached to its body, which allows it to be dragged
around."
  []
  (when-not @mousej
    (let [pt (mouse-world)
          fixt (first (query-at-point pt 1))]
      (when fixt
        (let [bod (body fixt)
              mj (mouse-joint! @ground-body bod pt
                               {:max-force (* 1000 (mass bod))})]
          (reset! mousej mj)
          (wake! bod))))))

(defn mouse-pressed
  "Dispatches according to the mouse button."
  []
  (case (quil/mouse-button)
    :left (left-mouse-pressed)
    :otherwise-ignore-it))

(defn mouse-released
  "Destroys the active mouse joint if it exists."
  []
  (when @mousej
    (do (destroy! @mousej)
        (reset! mousej nil))))

(defn left-mouse-dragged
  "Updates the mouse joint target point."
  []
  (when @mousej
    (let [pt (mouse-world)]
      (.setTarget @mousej (vec2 pt)))))

(defn right-mouse-dragged
  "Shifts the current view (camera)"
  []
  (let [[x y] (mouse-world)
        [ox oy] (pmouse-world)
        dx (- x ox)
        dy (- y oy)]
    (swap! camera (partial merge-with +)
           {:x-left (- dx) :y-bottom (- dy)})))

(defn mouse-dragged
  "Dispatches according to the mouse button."
  []
  (case (quil/mouse-button)
    :right (right-mouse-dragged)
    :left (left-mouse-dragged)
    :otherwise-ignore-it))

(defn zoom-camera
  "Factor multiplies the visible world distance."
  [factor]
  (swap! camera
         (fn [{:keys [width height x-left y-bottom]}]
           (let [cent-x (+ x-left (/ width 2))
                 cent-y (+ y-bottom (/ height 2))
                 new-width (* width factor)
                 new-height (* height factor)]
             {:width new-width
              :height new-height
              :x-left (- cent-x (/ new-width 2))
              :y-bottom (- cent-y (/ new-height 2))}))))

(defn key-press
  "Standard actions for key events"
  []
  (case (quil/raw-key)
    \  (swap! paused not)
    \. (do
         (swap! paused not)
         (draw)
         (swap! paused not))
    \= (zoom-camera (/ 1 1.5))
    \- (zoom-camera 1.5)
    :otherwise-ignore-it))
