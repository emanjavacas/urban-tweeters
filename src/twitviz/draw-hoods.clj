(ns twitviz.draw-hoods
  (:import [de.fhpotsdam.unfolding UnfoldingMap]
           [de.fhpotsdam.unfolding.utils MapUtils]
           [de.fhpotsdam.unfolding.geo Location]
           [de.fhpotsdam.unfolding.marker SimplePointMarker]
           [de.fhpotsdam.unfolding.providers StamenMapProvider Microsoft])
  (:use quil.core)
  (:require [quil.middleware :as m]
            [twitviz.utils :as u]
            [clojure.data.json :as json]))

(def berlin-hoods (u/fetch-hoods "resources/hoods/berlin.geojson.mod"))
(def berlin-hoods-sim (u/fetch-hoods "resources/hoods/berlin.json"))
(def amsterdam-hoods (u/fetch-hoods "resources/hoods/amsterdam.geojson"))
(def brussels-hoods (u/fetch-hoods "resources/hoods/bruxelles.json"))
(def antwerp-hoods (u/fetch-hoods "resources/hoods/antwerp.geojson.mod"))

(def city "berlin")
(defn draw-boundary [ps the-map]
  (doseq [[[y1 x1] [y2 x2]] (concat (partition 2 1 ps) (list (list (last ps) (first ps))))
          :let [loc1 (Location. x1 y1) loc2 (Location. x2 y2)]]
    (line (.x (.getScreenPosition the-map loc1)) (.y (.getScreenPosition the-map loc1))
          (.x (.getScreenPosition the-map loc2)) (.y (.getScreenPosition the-map loc2)))))

(defn draw-shape [ps the-map]
  (begin-shape)
  (doseq [[[y1 x1] [y2 x2]] (concat (partition 2 1 ps) (list (list (last ps) (first ps))))
          :let [loc1 (Location. x1 y1) loc2 (Location. x2 y2)]]
    (vertex (.x (.getScreenPosition the-map loc1)) (.y (.getScreenPosition the-map loc1)))
    (vertex (.x (.getScreenPosition the-map loc2)) (.y (.getScreenPosition the-map loc2))))
  (end-shape))

(defn setup []
  (let [the-map (UnfoldingMap. 
                 (quil.applet/current-applet) 
;                 (de.fhpotsdam.unfolding.providers.Microsoft$HybridProvider.)
                 (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
        center-lat (first ((keyword city) u/centers)) center-lon (second ((keyword city) u/centers))
        location (Location. center-lat center-lon)]
    (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) [the-map])
        (set-state!
         :map (doto the-map
                (.zoomAndPanTo location 14)
                (.setPanningRestriction location 5.0)
                (.setZoomRange 12 18)
                (.draw)))))

;; (defn draw []
;;   (let [the-map (state :map)
;;         location (.getLocation the-map (mouse-x) (mouse-y))]
;;     (.draw the-map)
;;     (stroke 200 200 50)
;;     (stroke-weight 6)
;;     (fill 10 10 10 1)
;;     (doseq [hood berlin-hoods]
;;       (draw-shape hood the-map))
;;     (fill 0)
;;     (text (str (.getLat location) ", " (.getLon location)) (mouse-x) (mouse-y))))

(defn draw []
  (let [the-map (state :map)
        location (.getLocation the-map (mouse-x) (mouse-y))]
    (.draw the-map)
    (stroke 200 200 50)
    (stroke-weight 6)
    (fill 10 10 10 10)
    (draw-shape (first berlin-hoods) the-map)
    (doseq [tw (take 1000 (u/tweet-stream "berlin"))
            :let [[y x] (u/tweet->coors tw)
                  loc (Location. y x)
                  px (.x (.getScreenPosition the-map loc))
                  py (.y (.getScreenPosition the-map loc))
                  [r g b] (if (u/inside? [x y] (first berlin-hoods))
                             [250 0 0]
                             [0 0 250])]]
      (stroke r g b)
      (ellipse px py 2 2))
    (fill 0)
    (text (str (.getLat location) ", " (.getLon location)) (mouse-x) (mouse-y))))

(defsketch mapping
  :title "Exploring Unfolding"
  :setup setup
  :draw draw
  :size [800 600]
  :renderer :opengl)
