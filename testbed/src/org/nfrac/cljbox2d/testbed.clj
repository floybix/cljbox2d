(ns org.nfrac.cljbox2d.testbed
  "Provides the scaffolding to run Box2D simulations with visual
  animation and interactive inputs (via mouse and keyboard). This is
  built on the Processing framework through the clojure wrapper
  [quil](https://github.com/quil/quil). Box2D timesteps are run
  synchronously with Processing (draw) timesteps: in this way we take
  advantage of Processing's queuing of inputs to avoid concurrency
  issues. In a real application you might want to run the drawing and
  physics in separate threads.

  In this namespace are drawing functions and input event handlers."
  (:require [org.nfrac.cljbox2d.core
             :refer [step! bodyseq fixtureseq body-type shape-type joint-type
                     body center world-coords radius mass query-at-point
                     destroy! user-data awake? wake! v2xy vec2
                     joint! alljointseq body-a body-b anchor-a anchor-b]]
            [quil.core :as quil])
  (:import (org.jbox2d.dynamics.joints MouseJoint)))

(def initial-state
  {:world nil
   :dt-secs (/ 1 30.0)
   :paused? false
   ;; the current view (location and scale) in world coordinates (m)
   :camera {:width 60 :height 40 :x-left -30 :y-bottom -10}
   :mouse-joint nil})

;; ## Drawing

(defn world-to-px-scale
  "A scaling factor on world coordinates to give pixels.
Fits the camera bounds into the window, expanding these
bounds if necessary to ensure an isometric aspect ratio."
  ([cam]
     (world-to-px-scale cam (quil/width) (quil/height)))
  ([cam px-width px-height]
     (let [xscale (/ px-width (:width cam))
           yscale (/ px-height (:height cam))]
       (min xscale yscale))))

(defn world-to-px
  "Convert a point in Box2d world coordinates to quil pixels."
  [cam [x y]]
  (let [scale (world-to-px-scale cam)
        x-left (:x-left cam)
        y-bottom (:y-bottom cam)
        y-top (+ y-bottom (:height cam))]
    [(* (- x x-left) scale)
     ;; quil has flipped y (0px at top)
     (* (- y-top y) scale)]))

(defn px-to-world
  "Convert a point in quil pixels to Box2d world coordinates."
  [cam [xp yp]]
  (let [scale (world-to-px-scale cam)
        x-left (:x-left cam)
        y-bottom (:y-bottom cam)
        y-top (+ y-bottom (:height cam))]
    [(+ (/ xp scale) x-left)
     ;; quil has flipped y (0px at top)
     (- y-top (/ yp scale))]))

(defn setup-style
  "Set common drawing style attributes"
  []
  (quil/background 0)
  (quil/stroke-weight 1))

(defn joint-style
  "Set drawing style for joints"
  []
  (let [blue (quil/color 155 155 255)]
    (quil/stroke blue)
    (quil/fill blue 127)))

(defn default-rgb
  [body]
  (case (body-type body)
    :static [100 255 100]
    :kinematic [100 100 255]
    :dynamic [255 200 200]))

(defn draw
  "Draw all shapes (fixtures) and joints in the Box2D world."
  [state]
  (let [world (:world state)
        cam (:camera state)
        ->px (partial world-to-px cam)]
    (setup-style)
    (joint-style)
    (doseq [jt (alljointseq world)
            :let [typ (joint-type jt)
                  body-a (body-a jt)
                  body-b (body-b jt)]]
      (case typ
        :revolute (let [anch (anchor-a jt)
                        center-a (center body-a)
                        center-b (center body-b)]
                    (quil/line (->px anch) (->px center-a))
                    (quil/line (->px anch) (->px center-b)))
        :distance (let [anch-a (anchor-a jt)
                        anch-b (anchor-b jt)]
                    (quil/line (->px anch-a) (->px anch-b)))
        :mouse (let [anch-b (anchor-b jt)
                     targ (v2xy (.getTarget ^MouseJoint jt))]
                 (quil/line (->px anch-b) (->px targ)))
        :otherwise-ignore-it
        ))
    (doseq [body (bodyseq world)
            :let [ud (user-data body)
                  ud (when (instance? clojure.lang.IDeref ud) (deref ud))
                  rgb (or (:rgb ud)
                          (default-rgb body))
                  color (apply quil/color rgb)
                  alpha (if (= :static (body-type body)) 64
                            (if (awake? body) 128 64))]]
      (quil/stroke color)
      (quil/fill color alpha)
      (doseq [fx (fixtureseq body)]
        (case (shape-type fx)
          :circle (let [[x y] (->px (center fx))
                        radius-px (* (radius fx) (world-to-px-scale cam))]
                    (quil/ellipse x y (* 2 radius-px) (* 2 radius-px)))
          (:edge
           :polygon) (let [pts (world-coords fx)
                           px-pts (map ->px pts)]
                       (quil/begin-shape)
                       (doseq [[x y] px-pts] (quil/vertex x y))
                       (quil/end-shape :close)))))))

;; ## input event handling

(defn left-mouse-pressed
  "Checks for fixtures at the mouse position. If one is found, creates
   a mouse joint attached to its body, which allows it to be dragged
   around."
  [state event]
  (if (:mouse-joint state)
    state
    (let [pt (px-to-world (:camera state) [(:x event) (:y event)])
          world (:world state)]
      (if-let [fixt (first (query-at-point world pt 1))]
        (let [bod (body fixt)
              ground-body (first (filter #(= :static (body-type %))
                                         (bodyseq world)))
              mj (joint! {:type :mouse
                          :body-a ground-body
                          :body-b bod
                          :target pt
                          :max-force (* 1000 (mass bod))})]
          (wake! bod)
          (assoc state :mouse-joint mj))
        state))))

(defn mouse-pressed
  "Dispatches according to the mouse button."
  [state event]
  (case (:button event)
    :left (left-mouse-pressed state event)
    state))

(defn mouse-released
  "Destroys the active mouse joint if it exists."
  [state event]
  (when-let [jt (:mouse-joint state)]
    (destroy! jt))
  (assoc state :mouse-joint nil))

(defn left-mouse-dragged
  "Updates the mouse joint target point."
  [state event]
  (when-let [jt ^MouseJoint (:mouse-joint state)]
    (let [pt (px-to-world (:camera state) [(:x event) (:y event)])]
      (.setTarget jt (vec2 pt))))
  state)

(defn right-mouse-dragged
  "Shifts the current view (camera)"
  [state event]
  (let [[x y] (px-to-world (:camera state) [(:x event) (:y event)])
        [px py] (px-to-world (:camera state) [(:p-x event) (:p-y event)])
        dx (- x px)
        dy (- y py)]
    (update-in state [:camera] (partial merge-with +)
               {:x-left (- dx) :y-bottom (- dy)})))

(defn mouse-dragged
  "Dispatches according to the mouse button."
  [state event]
  (case (:button event)
    :right (right-mouse-dragged state event)
    :left (left-mouse-dragged state event)
    state))

(defn zoom-camera
  "Factor multiplies the visible world distance."
  [camera factor]
  (let [{:keys [width height x-left y-bottom]} camera
        cent-x (+ x-left (/ width 2))
        cent-y (+ y-bottom (/ height 2))
        new-width (* width factor)
        new-height (* height factor)]
    {:width new-width
     :height new-height
     :x-left (- cent-x (/ new-width 2))
     :y-bottom (- cent-y (/ new-height 2))}))

(defn key-press
  "Standard actions for key events"
  [state event]
  (case (:raw-key event)
    \  (update-in state [:paused?] not)
    \. (update-in state [:world] step! (:dt-secs state))
    \= (update-in state [:camera] zoom-camera (/ 1 1.5))
    \- (update-in state [:camera] zoom-camera 1.5)
    state))
