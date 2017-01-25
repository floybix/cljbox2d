(ns org.nfrac.liquidfun.testbed.tests.faucet
  "A translation of the LiquidFun test Faucet."
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf :refer [body! joint!
                                                     particle-system!]]
            [quil.core :as quil :include-macros true]
            [quil.middleware])
  (:import (org.bytedeco.javacpp
            liquidfun$b2ParticleSystem
            liquidfun$b2ParticleDef)))

(def particleLifetimeMin 90.0)
(def particleLifetimeMax 120.0)
(def containerHeight 0.2)
(def containerWidth 1.0)
(def containerThickness 0.05)
(def faucetWidth 0.1)
(def faucetHeight 15.0)
(def faucetLength 2.0)
(def spoutWidth 1.1)
(def spoutLength 2.0)
(def maxParticleCount 3000)
(def emitRateChangeFactor 1.05)
(def emitRateMin 1.0)
(def emitRateMax 360.0)

(defn create-particle-colors []
  [(lf/particle-color [0xff, 0x80, 0x80, 0xff]) ;; red
   (lf/particle-color [0x80, 0xff, 0x80, 0xff]) ;; green
   (lf/particle-color [0x80, 0x80, 0xff, 0xff]) ;; blue
   (lf/particle-color [0xff, 0x8c, 0x00, 0xff]) ;; orange
   (lf/particle-color [0x00, 0xce, 0xd1, 0xff]) ;; turquoise
   (lf/particle-color [0xff, 0x00, 0xff, 0xff]) ;; magenta
   (lf/particle-color [0xff, 0xd7, 0x00, 0xff]) ;; gold
   (lf/particle-color [0x00, 0xff, 0xff, 0xff])]) ;; cyan

; pressure-strength 0.05
; repulsive-strength 1.0
; damping-strength 1.0

(defn setup []
  (let [world (lf/new-world)
        ps (particle-system! world
                             {:radius 0.035
                              :max-count maxParticleCount
                              :pressure-strength 1.0
                              :repulsive-strength 1.0
                              :destroy-by-age true})
        p-diameter (* (.GetRadius ps) 2)
        ground
        (let [height (+ containerHeight containerThickness)]
          (body! world {:type :static}
                 ;; the container / trough style sink
                 {:shape (lf/box (- containerWidth containerThickness)
                                 containerThickness [0 0])}
                 {:shape (lf/box containerThickness height
                                 [(- containerWidth) containerHeight])}
                 {:shape (lf/box containerThickness height
                                 [containerWidth containerHeight])}
                 ;; ground under the container to catch overflow
                 {:shape (lf/box (* containerWidth 3) containerThickness
                                 [0 (* containerThickness -2)])}
                 ;; enclosure
                 {:shape (let [hw (* containerWidth 3)
                               h (+ (* containerHeight faucetHeight)
                                    (* faucetLength p-diameter spoutLength))
                               y0 (- containerThickness)]
                           (lf/edge-chain [[(- hw) y0]
                                           [(- hw) h]
                                           [hw h]
                                           [hw y0]]))}))
        ;; the faucet spout
        spout
        (let [;; dimensions of the faucet in world units
              length (* faucetLength p-diameter spoutLength)
              width (* containerWidth faucetWidth spoutWidth)
              ;; height from the bottom of the container
              height (+ (* containerHeight faucetHeight) (* length 0.5))]
          (body! world {:type :static}
                 {:shape (lf/box p-diameter length [(- width) height])}
                 {:shape (lf/box p-diameter length [width height])}
                 {:shape (lf/box (- width p-diameter) p-diameter
                                 [0 (+ height length (- p-diameter))])}))
        pdef (lf/particle-def {})
        its (.CalculateReasonableParticleIterations world (/ 1 60.0))]
    (println "reasonable particle iterations:" its)
    (assoc bed/initial-state
      :world world
      :settings {:emit-rate 120
                 :remainder 0.0
                 :pdef pdef
                 :pcolors (create-particle-colors)
                 :white (lf/particle-color [255 255 255 255])}
      :particle-system ps
      :particle-iterations its
      :dt-secs (/ 1 60.0)
      :camera (bed/map->Camera {:width 4.5 :height 4 :center [0 2.0]}))))

(defn post-step
  [state]
  (let [dt (:dt-secs state)
        ps ^liquidfun$b2ParticleSystem (:particle-system state)
        length (* (.GetRadius ps) 2 faucetLength)
        {:keys [emit-rate remainder pcolors white] :as settings} (:settings state)
        pdef ^liquidfun$b2ParticleDef (:pdef settings)
        emit-n (+ remainder (* emit-rate dt))
        color (if (lf/particle-flag? (.flags pdef) :color-mixing)
                (nth pcolors (mod (int (* 0.3 (:time state))) (count pcolors)))
                white)]
    (dotimes [i (int emit-n)]
      (let [x (* containerWidth faucetWidth)
            y (+ (* containerHeight faucetHeight)
                 (* length (rand)))
            life (+ particleLifetimeMin
                    (* (rand) (- particleLifetimeMax particleLifetimeMin)))]
        (.SetPosition pdef x y)
        (.lifetime pdef life)
        (.color pdef color)
        (lf/particle! ps pdef)))
    (update state :settings assoc
            :remainder (- emit-n (int emit-n)))))

(defn step
  [state]
  (-> (bed/world-step state)
      (post-step)
      (bed/record-snapshot true)))

(defn draw
  [state]
  (bed/draw state true)
  (let []
    (quil/fill 255)
    (quil/text (str "Keys: (w) water, (q) powder\n"
                    "      (t) tensile, (v) viscous\n"
                    "      (c) color mixing, (s) static pressure\n"
                    "      (r) reactive, (p) repulsive\n"
                    "      (g) toggle gravity scale, (d) damping\n"
                    "      (2) increase radius, (1) decrease radius\n"
                    "      (+) increase flow, (-) decrease flow")
               10 10)))

(defn my-key-press
  [state event]
  (let [pdef ^liquidfun$b2ParticleDef (get-in state [:settings :pdef])
        ps ^liquidfun$b2ParticleSystem (:particle-system state)]
    (case (:key event)
      :w (do (.flags pdef (lf/kw->particle-flag :water))
             state)
      :q (do (.flags pdef (lf/kw->particle-flag :powder))
             state)
      :t (do (.flags pdef (lf/kw->particle-flag :tensile))
             state)
      :v (do (.flags pdef (lf/kw->particle-flag :viscous))
             state)
      :c (do (.flags pdef (lf/kw->particle-flag :color-mixing))
             state)
      :s (do (.flags pdef (lf/kw->particle-flag :static-pressure))
             state)
      :r (do (.flags pdef (lf/kw->particle-flag :reactive))
             state)
      :p (do (.flags pdef (lf/kw->particle-flag :repulsive))
             state)
      :g (do
           (.SetGravityScale ps (if (== 1.0 (.GetGravityScale ps))
                                  0.0 1.0))
           state)
      :d (do
           (.SetDamping ps (if (== 1.0 (.GetDamping ps))
                             0.0 1.0))
           state)
      :2 (do
           (.SetRadius ps (* 1.5 (.GetRadius ps)))
           state)
      :1 (do
           (.SetRadius ps (* (/ 1 1.5) (.GetRadius ps)))
           state)
      :? (do (println "pdef flags:" (.flags pdef))
           (println "particle flags:" (.GetAllParticleFlags ps))
           state)
      (:+ :=) (update-in state [:settings :emit-rate]
                         #(-> (* % emitRateChangeFactor)
                              (max emitRateMin)))
      :- (update-in state [:settings :emit-rate]
                    #(-> (/ % emitRateChangeFactor)
                         (min emitRateMax)))
      ;; otherwise pass on to testbed
      (bed/key-press state event))))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Faucet"
   :host "liquidfun"
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
