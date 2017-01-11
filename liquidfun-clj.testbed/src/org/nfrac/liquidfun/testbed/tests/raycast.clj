(ns org.nfrac.liquidfun.testbed.tests.raycast
  "A translation of Daniel Murphy's
   org.jbox2d.testbed.tests.RayCastTest"
  (:require [org.nfrac.liquidfun.testbed :as bed]
            [org.nfrac.liquidfun.core :as lf :refer [body! joint!]]
            [org.nfrac.liquidfun.vec2d :refer [polar-xy v-add TWOPI PI]]
            [quil.core :as quil :include-macros true]
            [quil.middleware])
  (:import (org.bytedeco.javacpp
            LongPointer)))

(def length 11.0)
(def eye [0 10])

(def initial-raycast
  {:mode :closest
   :angle 0.0
   :end-point eye})

(def shapes
  [(lf/polygon [[-0.5 0] [0.5 0] [0 1.5]])
   (lf/polygon [[-0.1 0] [0.1 0] [0 1.5]])
   (lf/polygon (for [i (range 8)
                     :let [angle (* TWOPI (/ i 8))]]
                 (polar-xy 0.5 angle)))
   (lf/box 0.5 0.5)
   (lf/circle 0.5)])

(defn setup []
  (let [world (lf/new-world)
        ground (body! world {:type :static}
                      {:shape (lf/edge [-40 0] [40 0])})
        id-ptrs (->> (range (count shapes))
                     (mapv #(-> (LongPointer. 1) (.put %))))]
    (assoc bed/initial-state
      :world world
      :id-pointers id-ptrs
      ::raycast initial-raycast
      ::bodies [])))

(defn record-raycast
  [state]
  (let [prev (::raycast state)
        prev-angle (:angle prev)
        angle (+ prev-angle (* 0.25 (/ PI 180)))
        end-point (v-add eye (polar-xy length angle))
        ignore (fn [fixt]
                 (if-let [ud (lf/user-data (lf/body-of fixt))]
                   (= 0 (.get (LongPointer. ud)))
                   false))
        hits (lf/raycast (:world state) eye end-point (:mode prev)
                          :ignore ignore)
        rc (assoc prev
             :angle angle
             :end-point end-point
             :hits hits)]
    (assoc state ::raycast rc)))

(defn step
  [state]
  (-> (bed/world-step state)
      (record-raycast)
      (bed/record-snapshot true [::raycast])))

(defn draw-additional
  [scene ->px]
  (let [rc (or (::raycast scene)
               initial-raycast)
        mode (:mode rc)
        hits (:hits rc)]
    (quil/fill 255)
    (quil/text (str "Press 1-5 to drop stuff, m to change the mode.\n"
                    "Polygon 1 is filtered.\n"
                    "Mode = " mode)
               10 10)
    (when (or (empty? hits)
              (= mode :all))
      (quil/line (->px eye)
                 (->px (:end-point rc))))
    (doseq [hit hits]
      (quil/line (->px eye) (->px (:point hit)))
      (let [[x-px y-px] (->px (:point hit))]
        (quil/ellipse x-px y-px 5 5)))))

(defn draw
  [state]
  (bed/draw state)
  (let [{:keys [snapshots steps-back camera]} state
        scene (nth snapshots steps-back nil)
        ->px (bed/world-to-px-fn camera)]
    (draw-additional scene ->px)))

(defn create-body
  [state index]
  (let [x (- (rand 20) 10)
        y (rand 20)
        theta (- (rand TWOPI) PI)
        id-ptr (nth (:id-pointers state) index)
        bod (body! (:world state) {:type :static
                                   :position [x y]
                                   :angle theta
                                   :user-data id-ptr}
                   {:shape (get shapes index)})]
    (update-in state [::bodies] conj bod)))

(defn my-key-press
  [state event]
  (let [char (:key event)]
    (case char
      (:1 :2 :3 :4 :5)
      (let [idx (dec (Integer. (name char)))]
        (create-body state idx))
      :m (let [mode (:mode (::raycast state))
               new-mode (case mode
                          :closest :all
                          :all :closest)]
           (assoc-in state [::raycast :mode] new-mode))
      :d (do (doseq [b (::bodies state)]
               (lf/destroy-body! b))
             (assoc state ::bodies []))
      ;; otherwise pass on to testbed
      (bed/key-press state event))))

(defn ^:export run
  "Run the test sketch."
  [& args]
  (quil/sketch
   :title "Raycast"
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
