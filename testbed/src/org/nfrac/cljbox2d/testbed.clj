(ns org.nfrac.cljbox2d.testbed
  "Provides some scaffolding to run Box2D simulations with visual
  animation and interactive inputs (via mouse and keyboard). This is
  built on the Processing framework through the clojure wrapper
  [quil](https://github.com/quil/quil). Box2D timesteps are run
  synchronously with Processing (draw) timesteps: in this way we take
  advantage of Processing's queuing of inputs to avoid concurrency
  issues. In a real application you might want to run the drawing and
  physics in separate threads.

  In this namespace are drawing functions and input event handlers."
  (:require [org.nfrac.cljbox2d.core :refer :all]
            [org.nfrac.cljbox2d.vec2d :refer [v-add]]
            [quil.core :as quil])
  (:import (org.jbox2d.dynamics.joints MouseJoint)))

(defrecord Camera [width height center])

(def initial-state
  {:world nil
   :dt-secs (/ 1 30.0)
   :paused? false
   :stepping? false
   :snapshots ()
   :keep-snapshots 1000
   :steps-back 0
   ;; the current view (location and scale) in world coordinates (m)
   :camera (map->Camera {:width 60 :height 40 :center [0 10]})
   :mouse-joint nil})

;; ## Snapshots - representing state as data for drawing

(defn record-snapshot
  "Generates a representation of the world for drawing and stores
   it in the list at key `:snapshots`. At most `:keep-snapshots` are
   kept. Argument `well-behaved?` asserts that Fixtures will not
   change, and that static bodies will not move: they can then be
   ignored for efficiency."
  [state well-behaved?]
  (let [world (:world state)
        prev-scene (first (:snapshots state))
        scene (snapshot-scene world hash prev-scene well-behaved?)
        keep-n (:keep-snapshots state)]
    (cond-> (update-in state [:snapshots] conj scene)
            ;; limit size of history buffer
            (>= (count (:snapshots state)) keep-n)
            (update-in [:snapshots] (partial take (* 0.5 keep-n)))
            ;; when simulating forward, always display current
            (pos? (:steps-back state))
            (assoc :steps-back 0))))

(defn step
  "Invokes a world simulation step. Also handles single stepping."
  [state]
  (cond-> (update-in state [:world] step! (:dt-secs state))
          ;; handle single stepping
          (:stepping? state)
          (assoc :stepping? false :paused? true)))

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

(defn world-to-px-fn
  "Returns a function to convert a point in Box2d world coordinates to
   quil pixels."
  [cam]
  (let [scale (world-to-px-scale cam)
        [cx cy] (:center cam)
        x-left (- cx (* 0.5 (:width cam)))
        y-bottom (- cy (* 0.5 (:height cam)))
        y-top (+ y-bottom (:height cam))]
    (fn [[x y]]
      [(* (- x x-left) scale)
       ;; quil has flipped y (0px at top)
       (* (- y-top y) scale)])))

(defn world-to-px
  "Convert a point in Box2d world coordinates to quil pixels."
  [cam [x y]]
  (let [f (world-to-px-fn cam)]
    (f [x y])))

(defn local-to-px
  "For use in transformed (body-local) drawing context.
   Converts a point in Box2d body-local coordinates to quil
   pixels."
  [px-scale [x y]]
  [(* x px-scale)
   ;; quil has flipped y (0px at top)
   (- (* y px-scale))])

(defn px-to-world-fn
  "Returns a function to convert a point in quil pixels to Box2d world
   coordinates."
  [cam]
  (let [scale (world-to-px-scale cam)
        [cx cy] (:center cam)
        x-left (- cx (* 0.5 (:width cam)))
        y-bottom (- cy (* 0.5 (:height cam)))
        y-top (+ y-bottom (:height cam))]
    (fn [[xp yp]]
      [(+ (/ xp scale) x-left)
       ;; quil has flipped y (0px at top)
       (- y-top (/ yp scale))])))

(defn px-to-world
  "Convert a point in quil pixels to Box2d world coordinates."
  [cam [xp yp]]
  (let [f (px-to-world-fn cam)]
    (f [xp yp])))

(defn draw-body
  [body-snap ->px px-scale]
  (let [{:keys [position angle fixtures]} body-snap
        ->loc-px (partial local-to-px px-scale)]
    (quil/with-translation (->px position)
      (quil/with-rotation [(- angle)]
        (doseq [fx-snap fixtures]
          (let [{:keys [radius center coords shape-type]} fx-snap]
            (case shape-type
              :circle (let [[x y] (->loc-px center)
                            radius-px (* radius px-scale)]
                        (quil/ellipse x y (* 2 radius-px) (* 2 radius-px)))
              (:edge :chain) (doseq [[pt1 pt2] (partition 2 1 (map ->loc-px coords))]
                               (quil/line pt1 pt2))
              :polygon (do
                         (quil/begin-shape)
                         (doseq [[x y] (map ->loc-px coords)]
                           (quil/vertex x y))
                         (quil/end-shape :close)))))))))

(defn draw-joint
  [jt-snap ->px]
  (let [{:keys [joint-type anchor-a anchor-b center-a center-b]} jt-snap]
    (case joint-type
      :revolute (do
                  (quil/line (->px anchor-a) (->px center-a))
                  (quil/line (->px anchor-a) (->px center-b)))
      ;; default:
      (do
        (quil/line (->px anchor-a) (->px anchor-b))))))

(defn default-colors
  []
  {:background (quil/color 0 0 0)
   :text (quil/color 255 255 255)
   :static (quil/color 100 255 100)
   :kinematic (quil/color 100 100 255)
   :dynamic (quil/color 255 200 200)
   :joint (quil/color 155 155 255)})

(defn draw-scene
  [scene cam colors show-help?]
  (let [->px (world-to-px-fn cam)
        px-scale (world-to-px-scale cam)]
    (quil/fill (:text colors))
    (quil/text-align :right)
    (if show-help?
      (quil/text (str "Drag bodies to move them.\n"
                      "Right-button drag to pan.\n"
                      "Mouse wheel or +/- to zoom.\n"
                      "Press space to pause, and press\n"
                      "</> to step in time (Shift=x10).")
                 (- (quil/width) 10) 10)
      (quil/text "Press \"?\""
                 (- (quil/width) 10) 10))
    (quil/text (str (apply format "(%.1f, %.1f)"
                           (px-to-world cam [(quil/mouse-x) (quil/mouse-y)])))
               (- (quil/width) 10)
               (- (quil/height) 5))
    (quil/text-align :left)
    (quil/stroke (:joint colors))
    (doseq [jt-snap (vals (:joints scene))]
      (draw-joint jt-snap ->px))
    (doseq [body-snap (vals (:bodies scene))
            :let [ud (:user-data body-snap)
                  color (if-let [[-r -g -b] (::rgb ud)]
                          (quil/color -r -g -b)
                          (colors (:body-type body-snap)))]]
      (let [alpha 64]
        (quil/stroke color)
        (quil/fill color alpha))
      (draw-body body-snap ->px px-scale))))

(defn draw
  "Draw all shapes (fixtures) and joints in the Box2D world."
  [state]
  (let [{:keys [world snapshots steps-back]} state
        scene (or (first snapshots)
                  ;; in case we are not recording snapshots:
                  (snapshot-scene world hash nil false))
        rewind-scene (when (pos? steps-back)
                       (nth snapshots steps-back nil))
        cam (:camera state)
        colors (::colors state (default-colors))]
    (quil/background (:background colors))
    (draw-scene scene cam colors (::show-help? state))
    (when rewind-scene
      (quil/fill (:background colors) 127)
      (quil/no-stroke)
      (quil/rect 0 0 (quil/width) (quil/height))
      (draw-scene rewind-scene cam colors (::show-help? state)))))

;; ## input event handlers

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
        (let [bod (body-of fixt)
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
    (update-in state [:camera :center] v-add [(- dx) (- dy)])))

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
  (let [{:keys [width height]} camera
        new-width (* width factor)
        new-height (* height factor)]
    (cond
     ;; don't zoom in closer than 1m
     (and (< new-width 1.0)
          (< factor 1.0))
     camera
     ;; don't zoom out further than 1000m
     (and (> new-width 1000)
          (> factor 1.0))
     camera
     :else
     (assoc camera
       :width new-width
       :height new-height))))

(defn align-camera
  "Moves camera so that the given world position [x y] is shown at the
   given pixel position."
  [camera [x y] [x-px y-px]]
  (let [[ox oy] (px-to-world camera [x-px y-px])]
    (update-in camera [:center] v-add [(- x ox) (- y oy)])))

(defn mouse-wheel
  [state rotation]
  (let [[x-px y-px] [(quil/mouse-x) (quil/mouse-y)]
        [x y] (px-to-world (:camera state) [x-px y-px])
        factor (if (pos? rotation) 1.02 (/ 1.02))]
    (-> state
        (update-in [:camera] zoom-camera factor)
        (update-in [:camera] align-camera [x y] [x-px y-px]))))

(defn key-press
  "Standard actions for key events"
  [state event]
  (case (:raw-key event)
    (\/ \?) (update-in state [::show-help?] not)
    \  (update-in state [:paused?] not)
    \. (if (pos? (:steps-back state))
         (update-in state [:steps-back] dec)
         (assoc state :stepping? true :paused? false))
    \> (update-in state [:steps-back] #(max 0 (- % 10)))
    \, (update-in state [:steps-back] inc)
    \< (update-in state [:steps-back] + 10)
    \= (update-in state [:camera] zoom-camera (/ 1 1.25))
    \- (update-in state [:camera] zoom-camera 1.25)
    state))
