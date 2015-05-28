(ns twitviz.protocols
  (:import [de.fhpotsdam.unfolding UnfoldingMap]
           [de.fhpotsdam.unfolding.utils MapUtils ScreenPosition]
           [de.fhpotsdam.unfolding.geo Location]
           [de.fhpotsdam.unfolding.providers StamenMapProvider Microsoft]
           [controlP5 ControlP5 ControlListener ControlEvent DropdownList])
  (:require [twitviz.utils :refer [draw-location sigmoid set-items rescaler]]
            [twitviz.data :refer [centers]])
  (:use quil.core)
  (:gen-class))

(set! *warn-on-reflection* true)

(defn listhint
  "determine return value to List to avoid reflection"
  ^java.util.List [x]
  [x])

(defn add-lang-listener [control id target-atom ls]
  (let [listener (reify ControlListener
                   (controlEvent [this event]
                     (if (.isGroup event)
                       (let [newval (.getValue (.getGroup ^ControlEvent event))]                         
                         (reset! target-atom (get (zipmap (range) ls) (int newval)))))))]
    (.addListener (.getGroup ^ControlP5 control id) ^ControlListener listener)
    listener))

(defn add-slider-listener [control id target-atom]
  (let [listener (reify ControlListener
                   (controlEvent [this event]
                     (let [newval (.getValue (.getController ^ControlEvent event))]
                       (reset! target-atom (int newval)))))]
    (.addListener (.getController ^ControlP5 control id) ^ControlListener listener)
    listener))

;;; Protocol
(defprotocol UnfoldingSketch
  "A protocol to be implemented by all visualization modes"
  (lang->color [this l1 l2] [this ls])
  (make-setup  [this])  
  (make-draw   [this]))

;;; SIGNATURES
;; Monolingualism     [grid city width height loc? gstate ls-idx max-ls] {:lang :alpha :map}
;; Bilingualism    [grid city width height loc? gstate ls-idx] {:lang1 :lang2 :alpha :beta :map}
;; Multilingualism [grid city width height loc? gstate modes] {:mode :alpha :beta :map}

(defrecord Monolingualism [grid city width height loc? gstate ls-idx max-ls]
  UnfoldingSketch
  (lang->color [this num-tws]
    (let [cur-lang @(:lang (:gstate this))
          max-val (get (:max-ls this) cur-lang 1);avoid div by zero
          r 255
          g ((rescaler 0 max-val 0 255) num-tws)
          b 0]
      [r g b]))

  (make-setup [this]
    (fn []
      (let [the-map (UnfoldingMap. (quil.applet/current-applet)
                                   (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
            loc (Location. ^Float (first ((keyword (:city this)) centers))
                           ^Float (second ((keyword (:city this)) centers)))
            control (ControlP5. (quil.applet/current-applet))
            ddl (doto (.addDropdownList control "lang" (- (:width this) 75) 10 30 300)
                   (set-items (:ls-idx this)))
            sldr (doto (.addSlider control "alpha" 0 255 50
                                   (- (:width this) 70) (- (:height this) 30) 50 20))]
        (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) (listhint the-map))        
        (add-lang-listener control "lang" (:lang (:gstate this)) (:ls-idx this))
        (add-slider-listener control "alpha" (:alpha (:gstate this)))
        (reset! (:map (:gstate this))
         (doto the-map
           (.zoomAndPanTo ^Location loc 13)
           (.setPanningRestriction ^Location loc 5.0)
           (.setZoomRange 12 18)
           (.draw))))))

  (make-draw [this]
    (fn []
      (let [the-map @(:map (:gstate this))]
        (.draw ^UnfoldingMap the-map)
        (when (:loc? this) (draw-location the-map))
        (no-stroke)
        (doseq [[[x y w h] ls] (:grid this) :when (and x y)]
          (let [pos1 (.getScreenPosition ^UnfoldingMap the-map (Location. ^Float x ^Float y))
                pos2 (.getScreenPosition ^UnfoldingMap the-map (Location. ^Float (+ x w) ^Float (+ y h)))
                px (.x ^ScreenPosition pos1) py (.y ^ScreenPosition pos1)
                pw (- (.x ^ScreenPosition pos2) px) ph (- (.y ^ScreenPosition pos2) py)
                num-tws (get ls @(:lang (:gstate this)) 0)
                [r g b] (lang->color this num-tws)]
            (fill r g b @(:alpha (:gstate this))) ; (if (and (zero? l1) (zero? l2)) 0 75)
            (rect px py pw ph)))))))

(defrecord Bilingualism [grid city width height loc? gstate ls-idx]
  UnfoldingSketch
  (lang->color [this l1 l2]
    (let [r 255
          g (cond (zero? l1) 255
                  (zero? l2) 0
                  :else (* (/ l2 (+ l1 l2)) 255))
          b (* 255 ((sigmoid @(:beta (:gstate this))) (- l2 l1)))]
      [r g b]))

    (make-setup [this]
      (fn []
        (let [the-map (UnfoldingMap. (quil.applet/current-applet)
                                     (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
              loc (Location. ^Float (first ((keyword (:city this)) centers))
                             ^Float (second ((keyword (:city this)) centers)))
              control (ControlP5. (quil.applet/current-applet))
              ddl1 (doto (.addDropdownList control "lang1" (- (:width this) 75) 10 30 300)
                     (set-items (:ls-idx this)))
              ddl2 (doto (.addDropdownList control "lang2" (- (:width this) 40) 10 30 300)
                     (set-items (:ls-idx this)))
              sldr1 (doto (.addSlider control "alpha" 0 255 50
                                      (- (:width this) 70) (- (:height this) 60) 50 20))
              sldr2 (doto (.addSlider control "beta" -1 1 0.3
                                      (- (:width this) 70) (- (:height this) 30) 50 20))]
          (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) (listhint the-map))        
          (add-lang-listener control "lang1" (:lang1 (:gstate this)) (:ls-idx this))
          (add-lang-listener control "lang2" (:lang2 (:gstate this)) (:ls-idx this))
          (add-slider-listener control "alpha" (:alpha (:gstate this)))
          (add-slider-listener control "beta" (:beta (:gstate this)))        
          (reset! (:map (:gstate this))
                  (doto the-map
                    (.zoomAndPanTo ^Location loc 13)
                    (.setPanningRestriction ^Location loc 5.0)
                    (.setZoomRange 12 18)
                    (.draw))))))
    
    (make-draw [this]
      (fn []
        (let [the-map @(:map (:gstate this))]
          (.draw ^UnfoldingMap the-map)
          (when (:loc? this) (draw-location the-map))
          (no-stroke)
          (doseq [[[x y w h] ls] (:grid this) :when (and x y)]
            (let [pos1 (.getScreenPosition ^UnfoldingMap the-map (Location. ^Float x ^Float y))
                  pos2 (.getScreenPosition ^UnfoldingMap the-map (Location. ^Float (+ x w) ^Float (+ y h)))
                  px (.x ^ScreenPosition pos1) py (.y ^ScreenPosition pos1)
                  pw (- (.x ^ScreenPosition pos2) px) ph (- (.y ^ScreenPosition pos2) py)
                  l1 (get ls @(:lang1 (:gstate this)) 0)
                  l2 (get ls @(:lang2 (:gstate this)) 0)
                  [r g b] (lang->color this l1 l2)]
              (fill r g b @(:alpha (:gstate this))) ; (if (and (zero? l1) (zero? l2)) 0 75)
              (rect px py pw ph)))))))

(defrecord Multilingualism [grid city width height loc? gstate modes]
  UnfoldingSketch
  (lang->color [this ls]
    (let [cur-mode @(:mode (:gstate this))
          cur-fn (get-in this [:modes cur-mode :fn])
          max-val (get-in this [:modes cur-mode :max])
          r 255
          g ((rescaler 0 max-val 0 255) (cur-fn ls))
          b 0]
      [r g b]))
  
  (make-setup [this]
    (fn []
      (let [the-map (UnfoldingMap. (quil.applet/current-applet)
                                   (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
            loc (Location. ^Float (first ((keyword (:city this)) centers))
                           ^Float (second ((keyword (:city this)) centers)))
            control (ControlP5. (quil.applet/current-applet))
            ddl1 (doto (.addDropdownList control "mode" (- (:width this) 75) 10 50 300)
                   (set-items (map str (keys (:modes this)))))
            sldr1 (doto (.addSlider control "alpha" 0 255 50
                                    (- (:width this) 70) (- (:height this) 60) 50 20))
            sldr2 (doto (.addSlider control "beta" -1 1 0.3
                                    (- (:width this) 70) (- (:height this) 30) 50 20))]
        (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) (listhint the-map))        
        (add-lang-listener control "mode" (:mode (:gstate this)) (keys (:modes this)))
        (add-slider-listener control "alpha" (:alpha (:gstate this)))
        (add-slider-listener control "beta" (:beta (:gstate this)))        
        (reset! (:map (:gstate this))
                (doto the-map
                  (.zoomAndPanTo ^Location loc 13)
                  (.setPanningRestriction ^Location loc 5.0)
                  (.setZoomRange 12 18)
                  (.draw))))))
  
  (make-draw [this]
    (fn []
      (let [the-map @(:map (:gstate this))]
        (.draw ^UnfoldingMap the-map)
        (when (:loc? this) (draw-location the-map))
        (no-stroke)
        (doseq [[[x y w h] ls] (:grid this) :when (and x y)]
          (let [
                pos1 (.getScreenPosition ^UnfoldingMap the-map (Location. ^Float x ^Float y))
                pos2 (.getScreenPosition ^UnfoldingMap the-map (Location. ^Float (+ x w) ^Float (+ y h)))
                px (.x ^ScreenPosition pos1) py (.y ^ScreenPosition pos1)
                pw (- (.x ^ScreenPosition pos2) px) ph (- (.y ^ScreenPosition pos2) py)
                [r g b] (lang->color this (vals ls))]
            (fill r g b @(:alpha (:gstate this)))
            (rect px py pw ph)))))))
