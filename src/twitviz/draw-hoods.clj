;; (ns twitviz.draw-hoods
;;   (:import [de.fhpotsdam.unfolding UnfoldingMap]
;;            [de.fhpotsdam.unfolding.utils MapUtils]
;;            [de.fhpotsdam.unfolding.geo Location]
;;            [de.fhpotsdam.unfolding.marker SimplePointMarker]
;;            [de.fhpotsdam.unfolding.providers StamenMapProvider Microsoft])
;;   (:use quil.core)
;;   (:require [twitviz.utils :refer :all]
;;             [clojure.data.json :as json]))

;; (def city "berlin")
;; (def hoods (fetch-hoods city))
;; (defn draw-boundary [ps the-map]
;;   (doseq [[[y1 x1] [y2 x2]] (concat (partition 2 1 ps) (list (list (last ps) (first ps))))
;;           :let [loc1 (Location. x1 y1) loc2 (Location. x2 y2)]]
;;     (line (.x (.getScreenPosition the-map loc1)) (.y (.getScreenPosition the-map loc1))
;;           (.x (.getScreenPosition the-map loc2)) (.y (.getScreenPosition the-map loc2)))))

;; (defn draw-shape [ps the-map]
;;   (begin-shape)
;;   (doseq [[[y1 x1] [y2 x2]] (concat (partition 2 1 ps) (list (list (last ps) (first ps))))
;;           :let [loc1 (Location. x1 y1) loc2 (Location. x2 y2)]]
;;     (vertex (.x (.getScreenPosition the-map loc1)) (.y (.getScreenPosition the-map loc1)))
;;     (vertex (.x (.getScreenPosition the-map loc2)) (.y (.getScreenPosition the-map loc2))))
;;   (end-shape))

;; (defn setup []
;;   (let [the-map (UnfoldingMap. 
;;                  (quil.applet/current-applet) 
;;                  (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
;;         loc (Location. (first ((keyword city) centers)) (second ((keyword city) centers))) ]
;;     (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) [the-map])
;;         (set-state!
;;          :map (doto the-map
;;                 (.zoomAndPanTo loc 12)
;;                 (.setPanningRestriction loc 5.0)
;;                 (.setZoomRange 12 18)
;;                 (.draw)))))

;; (defn draw []
;;   (let [the-map (state :map)]
;;     (.draw the-map)
;;     (stroke 200 200 50)
;;     (stroke-weight 6)
;;     (fill 10 10 10 10)
;;     (draw-shape (first hoods) the-map)
;;     (doseq [tw (take 1000 (tweet-stream city))
;;             :let [[y x] (tweet->coors tw)
;;                   loc (Location. y x)
;;                   px (.x (.getScreenPosition the-map loc))
;;                   py (.y (.getScreenPosition the-map loc))
;;                   [r g b] (if (inside? [x y] (first hoods))
;;                             [250 0 0]
;;                             [0 0 250])]]
;;       (stroke r g b)
;;       (ellipse px py 1 1))))

;; (defsketch mapping
;;   :title "Exploring Unfolding"
;;   :setup setup
;;   :draw draw
;;   :size [800 600]
;;   :renderer :opengl)
