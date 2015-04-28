(ns twitviz.entropy
  (:import [de.fhpotsdam.unfolding UnfoldingMap]
           [de.fhpotsdam.unfolding.utils MapUtils]
           [de.fhpotsdam.unfolding.geo Location]
           [de.fhpotsdam.unfolding.marker SimplePointMarker]
           [de.fhpotsdam.unfolding.providers StamenMapProvider Microsoft])
  (:use quil.core)
  (:require [quil.middleware :as m]
            [twitviz.utils :as u]))

(def nm 100)
(def city "berlin")
;; (def grid (u/create-grid nm city))
(def grid (u/load-grid "resources/berlin100.grid"))

(def max-range 
  (let [langs (for [[xyz m] grid :when xyz] m)]
    (apply max (map count langs))))

(defn entropy 
  "input must be a vector of raw (count) frequencies"
  [fs]
  (let [log-2 (Math/log 2)
        tot (reduce + fs)]
    (->> fs
         (map (fn [f]
                (let [rf (/ f tot)]
                  (-> (Math/log rf) 
                      (/ log-2) 
                      (* rf) 
                      Math/abs))))
         (reduce +))))

(def max-range (entropy (repeat max-range 1))) 

;; (defn lang->color 
;;   [max-range ls]
;;   (let [scaler (rescaler 0 max-range 0 255) 
;;         r 255
;;         g (scaler (count ls))
;;         b 0]
;;     [r g b]))

(let [scaler (u/rescaler 0 max-range 0 255)]
  (defn lang->color [ls f]
    (let [r 255
          g (scaler (f ls))
          b 0]
      [r g b])))

(def lang->color-memo (memoize lang->color))

(defn setup []
  (let [the-map (UnfoldingMap. 
                 (quil.applet/current-applet) 
;                 (de.fhpotsdam.unfolding.providers.Microsoft$HybridProvider.)
                 (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
        center-lat (first ((keyword city) u/centers))
        center-lon (second ((keyword city) u/centers))
        location (Location. center-lat center-lon)]
    (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) [the-map])
        (set-state!
         :map (doto the-map
                (.zoomAndPanTo location 14)
                (.setPanningRestriction location 5.0)
                (.setZoomRange 12 18)
                (.draw)))))

(defn draw []
  (let [the-map (state :map)
        location (.getLocation the-map (mouse-x) (mouse-y))]
    (.draw the-map)
    (doseq [[[x y side] ls] grid
            :when (and x y)] ;remove points outside the grid
      (let [pos1 (.getScreenPosition the-map (Location. x y))
            pos2 (.getScreenPosition the-map (Location. (+ x side) (+ y side)))
            px (.x pos1) py (.y pos1) w (- (.x pos2) px) h (- (.y pos2) py)
            [r g b] (lang->color-memo (vals ls) entropy)]
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
