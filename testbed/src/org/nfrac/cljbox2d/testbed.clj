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
  (:require [org.nfrac.cljbox2d.core
             :refer [step! bodyseq fixtureseq body-type shape-type joint-type
                     body-of center world-coords radius mass query-at-point
                     destroy! user-data awake? wake! v2xy vec2
                     joint! alljointseq body-a body-b anchor-a anchor-b]]
            [org.nfrac.cljbox2d.vec2d :refer [v-add]]
            [quil.core :as quil])
  (:import (org.jbox2d.dynamics.joints MouseJoint)))

(defrecord Camera [width height center])

(def initial-state
  {:world nil
   :dt-secs (/ 1 30.0)
   :paused? false
   ;; the current view (location and scale) in world coordinates (m)
   :camera (map->Camera {:width 60 :height 40 :center [0 10]})
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

(defn default-color
  [body]
  (case (body-type body)
    :static (quil/color 100 255 100)
    :kinematic (quil/color 100 100 255)
    :dynamic (quil/color 255 200 200)))

(defn draw-body
  [body ->px px-scale]
  (when-let [[-r -g -b] (::rgb (user-data body))]
    (let [color (quil/color -r -g -b)]
      (quil/stroke color)
      (quil/fill color 64)))
  (doseq [fx (fixtureseq body)]
    (case (shape-type fx)
      :circle (let [[x y] (->px (center fx))
                    radius-px (* (radius fx) px-scale)]
                (quil/ellipse x y (* 2 radius-px) (* 2 radius-px)))
      (:edge
       :chain) (doseq [[pt1 pt2] (partition 2 1 (map ->px (world-coords fx)))]
                 (quil/line pt1 pt2))
      :polygon
      (let [pts (world-coords fx)
            px-pts (map ->px pts)]
        (quil/begin-shape)
        (doseq [[x y] px-pts] (quil/vertex x y))
        (quil/end-shape :close)))))

(defn draw-joint
  [jt ->px]
  (let [typ (joint-type jt)
        body-a (body-a jt)
        body-b (body-b jt)]
    (case typ
      :revolute (let [anch (anchor-a jt)
                      center-a (center body-a)
                      center-b (center body-b)]
                  (quil/line (->px anch) (->px center-a))
                  (quil/line (->px anch) (->px center-b)))
      :mouse (let [anch-b (anchor-b jt)
                   targ (v2xy (.getTarget ^MouseJoint jt))]
               (quil/line (->px anch-b) (->px targ)))
      ;; default:
      (let [anch-a (anchor-a jt)
            anch-b (anchor-b jt)]
        (quil/line (->px anch-a) (->px anch-b))))))

(defn draw
  "Draw all shapes (fixtures) and joints in the Box2D world."
  [state]
  (let [world (:world state)
        cam (:camera state)
        ->px (world-to-px-fn cam)
        px-scale (world-to-px-scale cam)]
    (setup-style)
    (quil/text-align :right)
    (if (::show-help? state)
      (quil/text (str "Drag bodies to move them.\n"
                      "Right-button drag to pan.\n"
                      "Mouse wheel to zoom.\n"
                      "space to pause, > to step.")
                 (- (quil/width) 10) 10)
      (quil/text "Press \"?\""
                 (- (quil/width) 10) 10))
    (quil/text (str (apply format "(%.1f, %.1f)"
                           (px-to-world cam [(quil/mouse-x) (quil/mouse-y)])))
               (quil/width) (quil/height))
    (quil/text-align :left)
    (joint-style)
    (doseq [jt (alljointseq world)]
      (draw-joint jt ->px))
    (doseq [body (bodyseq world)
            :let [color (default-color body)]]
      (let [alpha 64]
        (quil/stroke color)
        (quil/fill color alpha))
      (draw-body body ->px px-scale))))

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
    (assoc camera
      :width new-width
      :height new-height)))

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
        factor (if (pos? rotation) 1.16 (/ 1.16))]
    (-> state
        (update-in [:camera] zoom-camera factor)
        (update-in [:camera] align-camera [x y] [x-px y-px]))))

(defn key-press
  "Standard actions for key events"
  [state event]
  (case (:raw-key event)
    (\/ \?) (update-in state [::show-help?] not)
    \  (update-in state [:paused?] not)
    \. (update-in state [:world] step! (:dt-secs state))
    \= (update-in state [:camera] zoom-camera (/ 1 1.25))
    \- (update-in state [:camera] zoom-camera 1.25)
    state))
