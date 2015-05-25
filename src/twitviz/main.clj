(ns twitviz.main
  (:import [de.fhpotsdam.unfolding UnfoldingMap]
           [de.fhpotsdam.unfolding.utils MapUtils ScreenPosition]
           [de.fhpotsdam.unfolding.geo Location]
           [de.fhpotsdam.unfolding.providers StamenMapProvider Microsoft CloudmadeProvider]
           [controlP5 ControlP5 ControlListener ControlEvent DropdownList])
  (:use quil.core)
  (:require [twitviz.utils :refer :all]
            [my-utils.io :refer [parse-number]]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(set! *warn-on-reflection* true)

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

(defn- add-ddl-listener [control id key gstate]
  (let [listener (reify ControlListener
                   (controlEvent [this event]
                     (if (.isGroup event)
                       (let [newval (.getValue (.getGroup ^ControlEvent event))]
                         (reset! (key gstate) (get (:int-ls gstate) (int newval)))))))]
    (.addListener (.getGroup ^ControlP5 control id) ^ControlListener listener)
    listener))

(defn- add-slider-listener [control id key gstate]
  (let [listener (reify ControlListener
                   (controlEvent [this event]
                     (let [newval (.getValue (.getController ^ControlEvent event))]
                       (reset! (key gstate) (int newval)))))]
    (.addListener (.getController ^ControlP5 control id) ^ControlListener listener)
    listener))

(defn- draw-location
  "draw coordinates at cursor"
  [^UnfoldingMap the-map]
  (let [^Location loc (.getLocation the-map (mouse-x) (mouse-y))]
    (fill 0)
    (text (str (.getLat loc) ", " (.getLon loc))
          (mouse-x)
          (mouse-y))))

(defn- set-items [^DropdownList ddl items]
  (doseq [[k v] items]
    (.addItem ddl k v)))

(defn- listhint ^java.util.List "avoid reflection with MapUtils" [x] [x])
(defn make-setup [gstate city]
  (fn []
    (let [the-map (UnfoldingMap. (quil.applet/current-applet)
                                 (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
          loc (Location. ^Float (first ((keyword city) centers)) ^Float (second ((keyword city) centers)))
          control (ControlP5. (quil.applet/current-applet))
          ddl1 (doto (.addDropdownList control "lang-1" 725 10 30 300) (set-items (:ls-int gstate)))
          ddl2 (doto (.addDropdownList control "lang-2" 760 10 30 300) (set-items (:ls-int gstate)))
          sldr (doto (.addSlider control "slider" -5 5 0.03 730 570 50 20))]
      (add-ddl-listener control "lang-1" :lang-1 gstate)
      (add-ddl-listener control "lang-2" :lang-2 gstate)
      (add-slider-listener control "slider" :a gstate)
      (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) (listhint the-map))
      (reset! (:map gstate)
       (doto the-map
         (.zoomAndPanTo loc 13)
         (.setPanningRestriction loc 5.0)
         (.setZoomRange 12 18)
         (.draw))))))

(defn make-draw [gstate grid loc?]
  (fn []
    (let [the-map @(:map gstate)]
      (.draw ^UnfoldingMap the-map)
      (when loc? (draw-location the-map))
      (no-stroke)
      (doseq [[[x y s _] ls] grid
              :when (and x y)] ;remove points outside the grid
        (let [pos1 (.getScreenPosition ^UnfoldingMap the-map (Location. ^Float x ^Float y))
              pos2 (.getScreenPosition ^UnfoldingMap the-map (Location. ^Float (+ x s) ^Float (+ y s)))
              px (.x ^ScreenPosition pos1) py (.y ^ScreenPosition pos1)
              w (- (.x ^ScreenPosition pos2) px) h (- (.y ^ScreenPosition pos2) py)
              l1 (get ls @(:lang-1 gstate) 0) 
              l2 (get ls @(:lang-2 gstate) 0) 
              [r g b] (lang->color l1 l2 @(:a gstate))]
          (fill r g b (if (and (zero? l1) (zero? l2)) 0 75))
          (rect px py w h))))))

;;; CLI
(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (clojure.string/join \newline errors)))

(defn exiting [status msg]
  (println msg)
  (System/exit status))

(defn usage [opts-summary]
  (->> ["" "                          *** TwitViz ***" ""
        "This is the help-menu of TwitViz, the main goal of this application is to visualize"
        "the spread of languages in urban landscapes based on only Twitter-data."
        "The core of the application is a GUI " ""
        "Usage: java -jar path/to/jar city [options]" ""
        "Options:"
        opts-summary ""
        "Datasets are available for the cities:" "amsterdam" "antwerp" "brussels" "berlin" ""
        "This application is written in Clojure." ""
        "Please refer to ... for a more detailed instructions."]
       (clojure.string/join \newline)))

(def cli-options
  [["-w" "--width" "Screen width" :default 1000 :parse-fn parse-number]
   ["-t" "--height" "Screen height" :default 1000 :parse-fn parse-number]
   ["-f" "--filter" "Give a low boundary for the number of tweets necessary to include the language in the GUI"
    :parse-fn parse-number :default 150]
   [nil "--location" "Select in order to show latitude and longitude of the mouse" :default true]   
   [nil "--path" "Hard-coded path to the desired grid-file"]
   ["-h" "--help" "Print this help and exit"]])

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    ;; handling error conditions
    (cond (:help options) (exiting 0 (usage summary))
          (and (not (:path options))
               (not (some #{(first arguments)} (map dekey (keys centers)))))
          (exiting 1 (usage summary))
          errors (exiting 1 (error-msg errors)))
    ;; init program
    (let [city (first arguments)
          grid   (fetch-grid (or (:path options) city))
          ls-int (zipmap (fetch-ls grid (:filter options)) (range))
          int-ls (into {} (for [[k v] ls-int] [v k]))
          gstate {:map (atom nil)         ;init map to nil
                  :lang-1 (atom (first (keys ls-int)))
                  :lang-2 (atom (second (keys ls-int)))
                  :int-ls int-ls
                  :ls-int ls-int
                  :a (atom 0.03)}]
      (defsketch TwitViz
        :title    (str "Displaying " city)
        :setup    (make-setup gstate city)
        :draw     (make-draw gstate grid (:location options))
        :size     [(:width options) (:height options)]
        :renderer :opengl))))
