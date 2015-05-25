(ns twitviz.bilingualism
  (:import [de.fhpotsdam.unfolding UnfoldingMap]
           [de.fhpotsdam.unfolding.utils MapUtils]
           [de.fhpotsdam.unfolding.geo Location]
           [de.fhpotsdam.unfolding.providers StamenMapProvider Microsoft OpenMapSurferProvider OpenStreetMap]
           [controlP5 ControlP5 ControlListener])
  (:use quil.core)
  (:require [twitviz.utils :refer :all]))

(def city "berlin")
(def grid (load-grid (str "resources/" city ".grid")))
(def ls-int (zipmap (fetch-ls grid 150) (range)))
(def int-ls (into {} (for [[k v] ls-int] [v k])))
(def gstate {:lang-1 (atom (first (keys ls-int)))
             :lang-2 (atom (second (keys ls-int)))
             :a (atom 0.03)})

;; (sort #(> (second %) (second %2))
;;       (zipmap (map #(* 255 ((sigmoid 0.02) %)) (range -100 100 20))
;;               (range -100 100 20)))

(defn lang->color
  "return a color in the red-yellow range mapping
   the r and y proportion to red and yellow respectively
   and the difference to the respective saturation"
  [l1 l2 a]
  ;; [255  ; set equal for all
  ;;  0-255 ; from red to yellow
  ;;  0-200] ; saturation
  (let [r 255
        g (cond (zero? l1) 255
                (zero? l2) 0
                :else (* (/ l2 (+ l1 l2)) 255))
        b (* 255 ((sigmoid a) (- l2 l1)))]
    [r g b]))

(defn add-ddl-listener [control id key gstate]
  (let [listener (reify ControlListener
                   (controlEvent [this event]
                     (if (.isGroup event)
                       (let [newval (.getValue (.getGroup event))]
                         (reset! (key gstate) (get int-ls (int newval)))))))]
    (.addListener (.getGroup control id) listener)
    listener))

(defn add-slider-listener [control id key gstate]
  (let [listener (reify ControlListener
                   (controlEvent [this event]
                     (let [newval (.getValue (.getController event))]
                       (reset! (key gstate) (int newval)))))]
    (.addListener (.getController control id) listener)
    listener))

(defn draw-location
  "draw coordinates at cursor"
  [the-map]
  (let [loc (.getLocation the-map (mouse-x) (mouse-y))]
    (fill 0)
    (text (str (.getLat loc) ", " (.getLon loc))
          (mouse-x)
          (mouse-y))))

(defn set-items [ddl items]
  (doseq [[k v] items]
    (.addItem ddl k v)))

(defn setup []
  (let [the-map (UnfoldingMap. (quil.applet/current-applet) 
                               (de.fhpotsdam.unfolding.providers.OpenStreetMap$OSMGrayProvider.))
        loc (Location. (first ((keyword city) centers))(second ((keyword city) centers)))
        control (ControlP5. (quil.applet/current-applet))
        ddl1 (doto (.addDropdownList control "lang-1" 725 10 30 300)
               (set-items ls-int))
        ddl2 (doto (.addDropdownList control "lang-2" 760 10 30 300)
               (set-items ls-int))
        sldr (doto (.addSlider control "slider" -5 5 0.03 730 570 50 20))]
    (add-ddl-listener control "lang-1" :lang-1 gstate)
    (add-ddl-listener control "lang-2" :lang-2 gstate)
    (add-slider-listener control "slider" :a gstate)
    (MapUtils/createDefaultEventDispatcher
     (quil.applet/current-applet) [the-map])
    (set-state!
     :map (doto the-map
            (.zoomAndPanTo loc 13)
            (.setPanningRestriction loc 5.0)
            (.setZoomRange 12 18)
            (.draw)))))

(defn draw []
  (let [the-map (state :map)]
    (.draw the-map)
    (no-stroke)
    (doseq [[[x y s _] ls] grid
            :when (and x y)] ;remove points outside the grid
      (let [pos1 (.getScreenPosition the-map (Location. x y))
            pos2 (.getScreenPosition the-map (Location. (+ x s) (+ y s)))
            px (.x pos1) py (.y pos1) w (- (.x pos2) px) h (- (.y pos2) py)
            l1 (get ls @(:lang-1 gstate) 0) 
            l2 (get ls @(:lang-2 gstate) 0) 
            [r g b] (lang->color l1 l2 0)]
        (fill r g b (if (and (zero? l1) (zero? l2)) 0 75))
        (rect px py w h)))))

(defsketch mapping
  :title "Exploring Unfolding"
  :setup setup
  :draw draw
  :size [800 600]
  :renderer :opengl)

;; (defn lang->color
;;   "return a color in the red-yellow range mapping
;;    the r and y proportion to red and yellow respectively
;;    and the difference to the respective saturation"
;;   [l1 l2 min-range max-range]
;;   ;; [255  ; set equal for all
;;   ;;  0-255 ; from red to yellow
;;   ;;  0-200] ; saturation
;;   (let [scaler (rescaler min-range max-range 0 200)
;;         r 255
;;         g (cond (zero? l1) 255
;;                 (zero? l2) 0
;;                 :else (* (/ l2 (+ l1 l2)) 255))
;;         b ;(scaler (Math/abs (- l1 l2)))
;;         (* 255 ((sigmoid @(:a gstate)) (- l2 l1)))
;;         ]
;;     [r g b]))

;; (def max-range NOT-NEEDED WITH SIGMOID FUNCTION
;;   (let [langs (for [[xyz m] grid :when xyz :when m] m)]
;;     (apply max (mapcat vals langs))))

;; (defn draw-loc []
;;   (let [the-map (state :map)]
;;     (.draw the-map)
;;     (draw-location the-map)
;;     (no-stroke)
;;     (doseq [[[x y s _] ls] grid
;;             :when (and x y)] ;remove points outside the grid
;;       (let [pos1 (.getScreenPosition the-map (Location. x y))
;;             pos2 (.getScreenPosition the-map (Location. (+ x s) (+ y s)))
;;             px (.x pos1) py (.y pos1) w (- (.x pos2) px) h (- (.y pos2) py)
;;             l1 (get ls @(:lang-1 gstate) 0) 
;;             l2 (get ls @(:lang-2 gstate) 0) 
;;             [r g b] (lang->color l1 l2 0 max-range)]
;;         (fill r g b (if (and (zero? l1) (zero? l2)) 0 75))
;;         (rect px py w h)))))
