;; (ns twitviz.main
;;   (:import [de.fhpotsdam.unfolding UnfoldingMap]
;;            [de.fhpotsdam.unfolding.utils MapUtils ScreenPosition]
;;            [de.fhpotsdam.unfolding.geo Location]
;;            [de.fhpotsdam.unfolding.providers StamenMapProvider Microsoft]
;;            [controlP5 ControlP5 ControlListener ControlEvent DropdownList])
;;   (:use quil.core)
;;   (:require [twitviz.utils :refer :all]
;;             [my-utils.io :refer [parse-number]]
;;             [clojure.tools.cli :refer [parse-opts]])
;;   (:gen-class))

;; (set! *warn-on-reflection* true)

;; (def mode-fns ["multilingual" "bilingual"])
;; (defrecord Grid [the-grid ls-idx city loc max])
;; (defn make-grid [infn ;min
;;                   city
;; ;                  max-fn
;;                   ]
;;   (let [the-grid (fetch-grid infn)
;;         ls (fetch-ls the-grid ;min
;;                            )
;;         max (max-range the-grid ;max-fn
;;                        count)
;;         loc (Location. ^Float (first ((keyword city) centers)) ^Float (second ((keyword city) centers)))]
;;     (Grid. the-grid ls city loc max)))
;; (def make-grid-memo ;infn ;min
;;                                         ;city;                 max-fn
;;   (memoize make-grid))

;; (defn set-items
;;   ([^DropdownList ddl vs]
;;    (set-items ddl vs (range (count vs))))
;;   ([^DropdownList ddl ks vs]
;;    (doseq [[k v] (zipmap ks vs)]
;;      (.addItem ddl k v))))

;; (defn add-lang-listener [control id key gstate]
;;   (let [listener (reify ControlListener
;;                    (controlEvent [this event]
;;                      (if (.isGroup event)
;;                        (let [newval (.getValue (.getGroup ^ControlEvent event))
;;                              grid @(:current-grid gstate)]                         
;;                          (reset! (key gstate) (get (zipmap (range) (:ls-idx grid)) (int newval)))))))]
;;     (.addListener (.getGroup ^ControlP5 control id) ^ControlListener listener)
;;     listener))

;; (defn add-grid-listener [control id key gstate]
;;   (let [listener (reify ControlListener
;;                    (controlEvent [this event]
;;                      (if (.isGroup event)
;;                        (let [newval (.getValue (.getGroup ^ControlEvent event))
;;                              [infn city] (get (zipmap (range) grids) newval)
;;                              ddl1 (.getGroup ^ControlP5 control "lang-1")
;;                              ddl2 (.getGroup ^ControlP5 control "lang-2")
;;                              newgrid (make-grid-memo infn city ;(:min gstate)
;;                                                 )]
;;                          (doto ^DropdownList ddl1 (.clear) (set-items (:ls-idx newgrid)))
;;                          (doto ^DropdownList ddl2 (.clear) (set-items (:ls-idx newgrid)))
;;                          (.panTo ^UnfoldingMap @(:map gstate) ^Location (:loc newgrid))
;;                          (reset! (key gstate) newgrid)))))]
;;     (.addListener (.getGroup ^ControlP5 control id) ^ControlListener listener)))

;; (defn add-mode-listener [control id key gstate]
;;   (let [listener (reify ControlListener
;;                    (controlEvent [this event]
;;                      (let [newval (.getValue (.getGroup ^ControlEvent event))]
;;                        (reset! (key gstate) (get (zipmap (range) mode-fns) (int newval))))))]
;;     (.addListener (.getGroup ^ControlP5 control id) ^ControlListener listener)
;;     listener))

;; (defn add-slider-listener [control id key gstate]
;;   (let [listener (reify ControlListener
;;                    (controlEvent [this event]
;;                      (let [newval (.getValue (.getController ^ControlEvent event))]
;;                        (reset! (key gstate) (int newval)))))]
;;     (.addListener (.getController ^ControlP5 control id) ^ControlListener listener)
;;     listener))

;; (defn draw-location
;;   "draw coordinates at cursor"
;;   [^UnfoldingMap the-map]
;;   (let [^Location loc (.getLocation the-map (mouse-x) (mouse-y))]
;;     (fill 0)
;;     (text (str (.getLat loc) ", " (.getLon loc))
;;           (mouse-x)
;;           (mouse-y))))

;; (defn listhint ^java.util.List [x] [x])
;; (defn make-setup [gstate]
;;   (fn []
;;     (let [the-map (UnfoldingMap. (quil.applet/current-applet)
;;                                  (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
;;           grid @(:current-grid gstate)
;;           control (ControlP5. (quil.applet/current-applet))
;;           ddl-lang-1 (doto (.addDropdownList control "lang-1" (- (:w gstate) 75) 10 30 300) (set-items (:ls-idx grid)))
;;           ddl-lang-2 (doto (.addDropdownList control "lang-2" (- (:w gstate) 40) 10 30 300) (set-items (:ls-idx grid)))
;;           ;; ddl-grid (doto (.addDropdownList control "grid" 75 10 200 200) (set-items (keys grids)))
;;           ddl-mode (doto (.addDropdownList control "mode" 300 10 50 200) (set-items mode-fns))
;;           sldr (doto (.addSlider control "slider" -5 5 0.03 (- (:w gstate) 70) (- (:h gstate) 30) 50 20))]
;;       ;; (add-grid-listener control "grid" :current-grid gstate)
;;       (add-lang-listener control "lang-1" :lang-1 gstate)
;;       (add-lang-listener control "lang-2" :lang-2 gstate)
;;       (add-mode-listener control "mode" :mode-fn gstate)
;;       (add-slider-listener control "slider" :a gstate)
;;       (MapUtils/createDefaultEventDispatcher (quil.applet/current-applet) (listhint the-map))
;;       (reset! (:map gstate)
;;        (doto the-map
;;          (.zoomAndPanTo ^Location (:loc grid) 13)
;;          (.setPanningRestriction ^Location (:loc grid) 5.0)
;;          (.setZoomRange 12 18)
;;          (.draw))))))

;; (defn make-draw [gstate loc?]
;;   (fn []
;;     (let [the-map @(:map gstate)
;;           grid @(:current-grid gstate)]
;;       (.draw ^UnfoldingMap the-map)
;;       (when loc? (draw-location the-map))
;;       (no-stroke)
;;       (doseq [[[x y s _] ls] (:the-grid grid)
;;               :when (and x y)] ;remove points outside the grid
;;         (let [pos1 (.getScreenPosition ^UnfoldingMap the-map (Location. ^Float x ^Float y))
;;               pos2 (.getScreenPosition ^UnfoldingMap the-map (Location. ^Float (+ x s) ^Float (+ y s)))
;;               px (.x ^ScreenPosition pos1) py (.y ^ScreenPosition pos1)
;;               w (- (.x ^ScreenPosition pos2) px) h (- (.y ^ScreenPosition pos2) py)
;;               l1 (get ls @(:lang-1 gstate) 0) 
;;               l2 (get ls @(:lang-2 gstate) 0)
;;               args (case @(:mode-fn gstate)
;;                      "bilingual" [l1 l2 @(:a gstate)]
;;                      "multilingual" [(vector (vals ls)) count (:max grid)])
;;               [r g b] (apply lang->color args)]
;;           (fill r g b ;(if (and (zero? l1) (zero? l2)) 0 75)
;;                 50)
;;           (rect px py w h))))))

;; ;;; CLI
;; (defn error-msg [errors]
;;   (str "The following errors occurred while parsing your command:\n\n"
;;        (clojure.string/join \newline errors)))

;; (defn exiting [status msg]
;;   (println msg)
;;   (System/exit status))

;; (defn usage [opts-summary]
;;   (->> ["" "                          *** TwitViz ***" ""
;;         "This is the help-menu of TwitViz, the main goal of this application is to visualize"
;;         "the spread of languages in urban landscapes based on only Twitter-data."
;;         "The core of the application is a GUI " ""
;;         "Usage: java -jar path/to/jar city [options]" ""
;;         "Options:"
;;         opts-summary ""
;;         "Datasets are available for the cities:" "amsterdam" "antwerp" "brussels" "berlin" ""
;;         "This application is written in Clojure." ""
;;         "Please refer to ... for a more detailed instructions."]
;;        (clojure.string/join \newline)))

;; (def cli-options
;;   [["-w" "--width" "Screen width" :default 800 :parse-fn parse-number]
;;    ["-t" "--height" "Screen height" :default 600 :parse-fn parse-number]
;;    ["-f" "--filter" "Give a low boundary for the number of tweets necessary to include the language in the GUI"
;;     :parse-fn parse-number :default 150]
;;    [nil "--location" "Select in order to show latitude and longitude of the mouse" :default true]   
;;    [nil "--path" "Hard-coded path to the desired grid-file"]
;;    ["-h" "--help" "Print this help and exit"]])

;; (defn -main [& args]
;;   (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
;;     ;; handling error conditions
;;     (cond (:help options) (exiting 0 (usage summary))
;;           errors (exiting 1 (error-msg errors)))
;;     ;; init program
;;     (let [infn "resources/berlin.grid"
;;           city (get grids infn)
;;           grid (make-grid-memo infn ;(:filter options)
;;                           city
;;                           )
;;           gstate {:map (atom nil)  ;init map to nil
;;                   :lang-1 (atom nil)
;;                   :lang-2 (atom nil)
;;                   :current-grid (atom grid)
;;                   :mode-fn (atom (first mode-fns))
;;                   :w (:width options) :h (:height options)
;;                   :a (atom 0.03)
;; ;                  :filter (:filter options)
;;                   }]
;;       (defsketch TwitViz
;;         :title    (str "Displaying " city)
;;         :setup    (make-setup gstate)
;;         :draw     (make-draw gstate (:location options))
;;         :size     [(:width options) (:height options)]
;;         :renderer :opengl))))

;; ;; (defn -main [& args]
;; ;;   (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
;; ;;     ;; handling error conditions
;; ;;     (cond (:help options) (exiting 0 (usage summary))
;; ;;           (and (not (:path options))
;; ;;                (not (some #{(first arguments)} (map dekey (keys centers)))))
;; ;;           (exiting 1 (usage summary))
;; ;;           errors (exiting 1 (error-msg errors)))
;; ;;     ;; init program
;; ;;     (let [city (first arguments)
;; ;;           grid   (fetch-grid (or (:path options) city))
;; ;;           ls-idx (index-list grid (:filter options))
;; ;;           gstate {:map (atom nil)         ;init map to nil
;; ;;                   :lang-1 (atom (first (keys ls-int)))
;; ;;                   :lang-2 (atom (second (keys ls-int)))
;; ;;                   :ls-idx ls-idx
;; ;;                   :w (:width options) :h (:height options)
;; ;;                   :a (atom 0.03)}]
;; ;;       (defsketch TwitViz
;; ;;         :title    (str "Displaying " city)
;; ;;         :setup    (make-setup gstate city)
;; ;;         :draw     (make-draw gstate grid (:location options))
;; ;;         :size     [(:w gstate) (:h gstate)]
;; ;;         :renderer :opengl)))) 
