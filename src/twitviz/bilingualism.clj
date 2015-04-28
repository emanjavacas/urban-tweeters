(ns twitviz.bilingualism
  (:import [de.fhpotsdam.unfolding UnfoldingMap]
           [de.fhpotsdam.unfolding.utils MapUtils]
           [de.fhpotsdam.unfolding.geo Location]
           [de.fhpotsdam.unfolding.marker SimplePointMarker]
           [de.fhpotsdam.unfolding.providers StamenMapProvider Microsoft]
           [java.awt.event KeyEvent]
;           [controlP5 ControlEvent ControlP5]
)
  (:use quil.core)
  (:require [quil.middleware :as m]
            [twitviz.utils :refer :all]))

(defprotocol Move
  (current [coll])
  (forward [coll])
  (backward [coll]))

(defrecord CircleSeq [coll start]
  Move
  (current [this] (nth (:coll this) @(:start this)))
  (forward [this]
    (let [n @(:start this)
          coll (:coll this)]
      (if (> (inc n) (count this))
        (nth coll (reset! (:start this) 0))
        (nth coll (swap! (:start this) inc)))))
  (backward [this]
    (let [n @(:start this)
          coll (:coll this)]
      (if (zero? n)
        (nth coll (reset! (:start this) (dec (count coll))))
        (nth coll (swap! (:start this) dec))))))

(defn make-circle-seq
  ([coll] (make-circle-seq coll 0))
  ([coll start]
   (let [idx (atom start)]
     (CircleSeq. coll idx))))

(declare buttons)
(def city "antwerp")
(def grid (load-grid (str "resources/" city ".grid")))
(def ls (fetch-ls grid 150))
(def max-range 
  (let [langs (for [[xyz m] grid :when xyz :when m] m)]
    (apply max (mapcat vals langs))))

(defn get-lang
  "retrieves language associated with a given key"
  [key]
  (current (state key)))

(defn make-button
  "defines the button structure"
  [key x y w h r b g]
  {:bounds [x y w h] :key key :bg [r b g]})

(defn button-handler
  "defines action on pressed button"
  [{:keys [bounds key]} x y]
  (if (in-rect? x y bounds)
    (forward (state key))))

(defn draw-button
  "displays button and current language"
  [{[r b g] :bg [x y w h] :bounds key :key}]
  (let [l (get-lang key)]
    (fill r g b)
    (rect x y w h)
    (fill 0)
    (text l (+ (/ w 4) x) (+ (/ h 1.5) y))))

(def buttons
  [(make-button :l1 725 10 30 20 67 211 227)
   (make-button :l2 765 10 30 20 67 211 227)])

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
        g (cond (zero? l1) 255 
                (zero? l2) 0
                :else (* (/ l2 (+ l1 l2)) 255))
        b (scaler (Math/abs (- l1 l2)))]
    [r g b]))

(defn draw-location
  "draw coordinates at cursor"
  [the-map]
  (let [loc (.getLocation the-map (mouse-x) (mouse-y))]
    (fill 0)
    (text (str (.getLat loc) ", " (.getLon loc))
          (mouse-x)
          (mouse-y))))

(defn mouse-pressed 
  "triggers handler when mouse pressed" []
  (let [x (mouse-x) y (mouse-y)]
    (doseq [b buttons] (button-handler b x y))))

(defn key-pressed []
  (cond
    (= (key-code) KeyEvent/VK_A) (backward (state :l1))
    (= (key-code) KeyEvent/VK_D) (forward (state :l1))
    (= (key-code) KeyEvent/VK_W) (forward (state :l2))
    (= (key-code) KeyEvent/VK_S) (backward (state :l2))))

(defn setup []
  (let [the-map (UnfoldingMap. 
                 (quil.applet/current-applet) 
                 (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
        loc (Location. (first ((keyword city) centers)) (second ((keyword city) centers)))]
    (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) [the-map])
        (set-state!
         :l1 (make-circle-seq ls)
         :l2 (make-circle-seq ls 1)
         :map (doto the-map
                (.zoomAndPanTo loc 14)
                (.setPanningRestriction loc 5.0)
                (.setZoomRange 12 18)
                (.draw)))))

(defn draw []
  (let [the-map (state :map)]
    (.draw the-map)
;    (draw-location the-map)
    (no-stroke)
    (doseq [[[x y side] ls] grid
            :when (and x y)] ;remove points outside the grid
      (let [pos1 (.getScreenPosition the-map (Location. x y))
            pos2 (.getScreenPosition the-map (Location. (+ x side) (+ y side)))
            px (.x pos1) py (.y pos1) w (- (.x pos2) px) h (- (.y pos2) py)
            l1 (or (get ls (get-lang :l1)) 0) 
            l2 (or (get ls (get-lang :l2)) 0) 
            [r g b] (lang->color l1 l2 0 max-range)]
        (fill r g b (if (and (zero? l1) (zero? l2)) 0 75))
        (rect px py w h)))
    (stroke 100 100 100)
    (doseq [b buttons] (draw-button b))))

(defsketch mapping
  :title "Exploring Unfolding"
  :setup setup
  :draw draw
  :mouse-pressed mouse-pressed
  ;; :key-pressed key-pressed
  :size [800 600]
  :renderer :opengl)

;;; TIMING
;;; 
;; (time (def grid-t
;;   (let [[min-lat min-lon max-lat max-lon] (boxes :brussels)
;;         w (/ (- max-lat min-lat) nm)
;;         h (/ (- max-lon min-lon) nm)
;;         tiles (for [lat (range min-lat max-lat w)
;;                     lon (range min-lon max-lon w)]
;;                 [lat lon w])
;;         ps (for [tw (tweet-stream "brussels")
;;                  :let [l (tweet->lang tw)]
;;                  :when (some #{l} ["fr" "nl"])]
;;              [l (tweet->coors tw)])]
;;     (ps->grid tiles ps))))
;; "Elapsed time: 477639.751 msecs"

;; (time (def grid-t
;;   (let [[min-lat min-lon max-lat max-lon] (boxes :brussels)
;;         w (/ (- max-lat min-lat) nm)
;;         h (/ (- max-lon min-lon) nm)
;;         tiles (for [lat (range min-lat max-lat w)
;;                     lon (range min-lon max-lon w)]
;;                 [lat lon w])
;;         ps (for [tw (tweet-stream "brussels")
;;                  :let [l (tweet->lang tw)]
;;                  :when (some #{l} ["fr" "nl"])]
;;              [l (tweet->coors tw)])]
;;     (ps->grid tiles (sort-by #(first (second %)) ps)))))
;; "Elapsed time: 474977.151 msecs"

;; (time (def s (doseq [tw (tweet-stream "brussels")
;;                    :let [l (tweet->lang tw)]
;;                    :when (some #{l} ["fr" "nl"])]
;;                [l (tweet->coors tw)])))
;; "Elapsed time: 33278.716 msecs"


;;; ControlP5
;;; 
;; (defn controlEvent [event]
;;   (if (.isGroup event)
;;     (reset! (state :v) (.getValue (.getGroup event)))
;;     nil))

;; (defn set-items [ddl items]
;;   (doseq [[k v] items]
;;     (.addItem ddl k v)))

;; (defn setup []
;;   (let [control (ControlP5. (quil.applet/current-applet))]
;;     (set-state!
     ;; :v (atom 1)
;;      :ddl1 (doto (.addDropdownList control "L1" 725 10 30 80)
;;              (set-items ls->int)
;;              (.setId 1))
;;      :ddl2 (doto (.addDropdownList control "L2" 760 10 30 80)
;;              (set-items ls->int)
;;              (.setId 2)))))

;; (defn draw []
;;   (let [v @(state :v)]
;;     (background 200 200 200)
;;     (text (str 123) 250 310)))

;;; OLDER
;; (defn setup []
;;   (let [the-map (UnfoldingMap. 
;;                  (quil.applet/current-applet) 
;;                  (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
;;         location (Location. (first (:brussels centers))
;;                             (second (:brussels centers)))]
;;     (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) [the-map])
;;         (set-state!
;;          :l1 "fr"
;;          :l2 "nl"
;;          :map (doto the-map
;;                 (.zoomAndPanTo location 14)
;;                 (.setPanningRestriction location 5.0)
;;                 (.setZoomRange 12 18)
;;                 (.draw)))))

;; (defn draw []
;;   (let [the-map (state :map)
;;         location (.getLocation the-map (mouse-x) (mouse-y))
;;         langs (for [[xyz m] grid
;;                     :when xyz] m)
;;         max-range (max (:fr (last (sort-by :fr langs))) 
;;                        (:nl (last (sort-by :nl langs))))]
;;     (.draw the-map)
;;     (doseq [[[x y side] ls] grid
;;             :when (and x y)] ;remove points outside the grid
;;       (let [pos1 (.getScreenPosition the-map (Location. x y))
;;             pos2 (.getScreenPosition the-map (Location. (+ x side) (+ y side)))
;;             px (.x pos1) py (.y pos1) w (- (.x pos2) px) h (- (.y pos2) py)
;;             [r g b] (lang->color (or (:fr ls) 0) 
;;                                  (or (:nl ls) 0) 0 max-range)]
;;         (no-stroke)
;;         (fill r g b 50)
;;         (rect px py w h)))
;;     (fill 0)
;;     (text (str (.getLat location) ", " (.getLon location)) (mouse-x) (mouse-y))))
