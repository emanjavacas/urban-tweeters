(ns twitviz.pair-langs
  (:import [de.fhpotsdam.unfolding UnfoldingMap]
           [de.fhpotsdam.unfolding.utils MapUtils]
           [de.fhpotsdam.unfolding.geo Location]
           [de.fhpotsdam.unfolding.marker SimplePointMarker]
           [de.fhpotsdam.unfolding.providers StamenMapProvider Microsoft])
  (:use quil.core)
  (:require [quil.middleware :as m]
            [twitviz.utils :refer :all]))

(def nm 100)
(def city "brussels")

(defn lang->color
  "return a color in the red-yellow range mapping
   the r and y proportion to red and yellow respectively
   and the difference to the respective saturation"
  [l1 l2 min-range max-range]
  ;; [255  ; set equal for all
  ;;  0-255 ; from red to yellow
  ;;  0-200] ; saturation
  (let [scaler (rescaler min-range max-range 0 200)
        r 255
        g (cond (zero? l2) 0 
                (zero? l1) 255
                :else (* (/ l2 (+ l1 l2)) 255))
        b (scaler (Math/abs (- l1 l2)))]
    [r g b]))



(defn setup []
  (let [the-map (UnfoldingMap. 
                 (quil.applet/current-applet) 
                 (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
        location (apply Location. (:brussels centers))]
    (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) [the-map])
        (set-state!
         :map (doto the-map
                (.zoomAndPanTo location 14)
                (.setPanningRestriction location 5.0)
                (.setZoomRange 12 18)
                (.draw)))))

(defn draw []
  (let [the-map (state :map)
        location (.getLocation the-map (mouse-x) (mouse-y))
        langs (for [[xyz m] grid
                    :when xyz] m)
        max-range (max (:fr (last (sort-by :fr langs))) 
                       (:nl (last (sort-by :nl langs))))]
    (.draw the-map)
    (doseq [[[x y side] ls] grid
            :when (and x y)] ;remove points outside the grid
      (let [pos1 (.getScreenPosition the-map (Location. x y))
            pos2 (.getScreenPosition the-map (Location. (+ x side) (+ y side)))
            px (.x pos1) py (.y pos1) w (- (.x pos2) px) h (- (.y pos2) py)
            [r g b] (lang->color (or (:fr ls) 0) 
                                 (or (:nl ls) 0) 0 max-range)]
        (no-stroke)
        (fill r g b 50)
        (rect px py w h)))
    (fill 0)
    (text (str (.getLat location) ", " (.getLon location)) (mouse-x) (mouse-y))))

(defsketch mapping
  :title "Exploring Unfolding"
  :setup setup
  :draw draw
  :size [800 600]
  :renderer :opengl)

