(ns twitviz.sliders
  (:import [controlP5 ControlP5 ControlListener])
  (:use quil.core))

(def gstate {:ddl1 (atom "1")
             :ddl2 (atom "2")})

(def items (zipmap (map str (range 10)) (range 10 20)))
(def smeti (into {} (for [[k v] items] [v k])))

(defn add-ddl-listener [control id key gstate]
  (let [listener (reify ControlListener
                   (controlEvent [this event]
                     (if (.isGroup event)
                       (let [newval (.getValue (.getGroup event))]
                         (reset! (key gstate) (int newval))))))]
    (.addListener (.getGroup control id) listener)
    listener))

(defn set-items [ddl items]
  (doseq [[k v] items]
    (.addItem ddl k v)))

(defn setup []
  (let [control (ControlP5. (quil.applet/current-applet))
        ddl1 (doto (.addDropdownList control "L1" 725 10 30 80)
             (set-items items))
        ddl2 (doto (.addDropdownList control "L2" 760 10 30 80)
             (set-items items))]
    (add-ddl-listener control "L1" :ddl1 gstate)
    (add-ddl-listener control "L2" :ddl2 gstate)))

(defn draw []
  (background 128)
  (let [v1 @(:ddl1 gstate) v2 @(:ddl2 gstate)]
    (text (str (get smeti v1) " and " (get smeti v2)) 100 100)))

(defsketch sliders
  :title "Check that colors!"
  :setup setup
  :draw draw
  :size [800 600])

;;; ControlP5
;;; 
;; (defn controlEvent [event]
;;   (if (.isGroup event)
;;     (reset! (state :v) (.getValue (.getGroup event)))
;;     nil))

;; (defn set-items [ddl items]
;;   (doseq [[k v] items]
;;     (.addItem ddl k v)))

;; (defn make-button
;;   "defines the button structure"
;;   [key x y w h r b g]
;;   {:bounds [x y w h] :key key :bg [r b g]})

;; (defn button-handler
;;   "defines action on pressed button"
;;   [{:keys [bounds key]} x y]
;;   (if (in-rect? x y bounds)
;;     (forward (state key))))

;; (defn draw-button
;;   "displays button and current language"
;;   [{[r b g] :bg [x y w h] :bounds key :key}]
;;   (let [l (get-lang key)]
;;     (fill r g b)
;;     (rect x y w h)
;;     (fill 0)
;;     (text l (+ (/ w 4) x) (+ (/ h 1.5) y))))

;; (def buttons
;;   [(make-button :l1 725 10 30 20 67 211 227)
;;    (make-button :l2 765 10 30 20 67 211 227)])

;; (defn mouse-pressed 
;;   "triggers handler when mouse pressed" []
;;   (let [x (mouse-x) y (mouse-y)]
;;     (doseq [b buttons] (button-handler b x y))))

;; (defn key-pressed []
;;   (cond
;;     (= (key-code) KeyEvent/VK_A) (backward (state :l1))
;;     (= (key-code) KeyEvent/VK_D) (forward (state :l1))
;;     (= (key-code) KeyEvent/VK_W) (forward (state :l2))
;;     (= (key-code) KeyEvent/VK_S) (backward (state :l2))))

;; (defn get-lang
;;   "retrieves language associated with a given key"
;;   [key]
;;   (current (state key)))

;; (defn draw []
;;   (let [the-map (state :map)]
;;     (.draw the-map)
;; ;    (draw-location the-map)
;;     (no-stroke)
;;     (doseq [[[x y side] ls] grid
;;             :when (and x y)] ;remove points outside the grid
;;       (let [pos1 (.getScreenPosition the-map (Location. x y))
;;             pos2 (.getScreenPosition the-map (Location. (+ x side) (+ y side)))
;;             px (.x pos1) py (.y pos1) w (- (.x pos2) px) h (- (.y pos2) py)
;;             l1 (or (get ls (get-lang :l1)) 0) 
;;             l2 (or (get ls (get-lang :l2)) 0) 
;;             [r g b] (lang->color l1 l2 0 max-range)]
;;         (fill r g b (if (and (zero? l1) (zero? l2)) 0 75))
;;         (rect px py w h)))
;;     (stroke 100 100 100)
;;     (doseq [b buttons] (draw-button b))))

;; (defn setup []
;;   (let [the-map (UnfoldingMap. 
;;                  (quil.applet/current-applet) 
;;                  (de.fhpotsdam.unfolding.providers.StamenMapProvider$TonerBackground.))
;;         loc (Location. (first ((keyword city) centers))
;;                        (second ((keyword city) centers)))]
;;     (MapUtils/createDefaultEventDispatcher
;;      (quil.applet/current-applet) [the-map])
;;         (set-state!
;;          :l1 (make-circle-seq ls)
;;          :l2 (make-circle-seq ls 1)
;;          :map (doto the-map
;;                 (.zoomAndPanTo loc 14)
;;                 (.setPanningRestriction loc 5.0)
;;                 (.setZoomRange 12 18)
;;                 (.draw)))))
;; (defn setup []
;;   (let [control (ControlP5. (quil.applet/current-applet))
;;         r (.addSlider control "r" 0 250 0 650 10 100 20)
;;         g (.addSlider control "g" 0 250 0 650 30 100 20)
;;         b (.addSlider control "b" 0 250 0 650 50 100 20)]
;;     (do (add-slider-listener control "r" gstate)
;;         (add-slider-listener control "g" gstate)
;;         (add-slider-listener control "b" gstate))))
;; (def gstate {:r (atom 0)
;;              :g (atom 0)
;;              :b (atom 0)})
;; (defn add-slider-listener [control id key gstate]
;;   (let [listener (reify ControlListener
;;                    (controlEvent [this event]
;;                      (let [newval (.getValue (.getController event))]
;;                        (reset! (key gstate) (int newval)))))]
;;     (.addListener (.getController control id) listener)
;;     listener))
