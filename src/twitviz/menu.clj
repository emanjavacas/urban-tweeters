(ns twitviz.menu
  (:require [seesaw.core :refer :all]
            [seesaw.bind :as bind]
            [seesaw.swingx :refer :all]
            [seesaw.table :refer :all]
            [twitviz.utils :refer :all]))

(def app-data {:width (atom 800) :height (atom 600) :filter (atom 50) :loc? (atom true)
               :mode (atom :multilingual) :mode-fn (atom count)
               :grid (atom (first grids))})

(defn run-twitviz [{:keys [width height filter loc? mode mode-fn grid]}]
  )

(defn explorer-content [title desc content]
  (border-panel
   :id :explorer-content
   :hgap 5 :vgap 5 :border 5
   :north (header :title title
                  :description (str "<html>" (clojure.string/join "<br>" desc) "</html>"))
   :center content))

(def init-menu
  (explorer-content
   "Initialization menu"
   ["Click on run in order to start the application."
    "<i>You can also select the desired setup in the menu on the left handside</i>."]
   (vertical-panel
    :items [(button
             :text "Run!"
             :mnemonic \R
             :halign :center :valign :center
             :selected? false
             :listen [:action (fn [e] (alert "RUN!"))])])))

(def descriptions
  (titled-panel
   :title "Description of the grid file"
   :content (label-x
             :id :descriptions
             :wrap-lines? true
             :h-text-position :center
             :v-text-position :center
             :text (get-description (first grids)))))

(def files-table
  (scrollable
   (table-x
    :id :files-table
    :model [:columns [:city :file :size]
            :rows  (map (fn [g] (zipmap [:city :file :size]
                                        ((juxt find-city identity find-size) g)))
                        grids)]
    :horizontal-scroll-enabled? true
    :selection-mode :single)))

(def grid-files
  (explorer-content
   "Available Grid files"
   ["<i>A description of the selected grid file is being shown below</i>."
    "<i>The selected file will be used for the visualization after initializing it from the <b>Init menu</b>"]
   (vertical-panel
    :items [files-table :separator descriptions])))

(defn create-widget [id desc-text widget-type out-fn & model]
  (let [head (label-x :text (apply str (rest (str id))) :wrap-lines? true :font "ARIAL-BOLD-15")
        desc (label-x :text desc-text :wrap-lines? true)
        widget (case widget-type
                 :text (text :id id :columns 3 :text (str @(id app-data))
                             :listen [:action (fn [e] (reset! (id app-data) (out-fn (text e))))])
                 :combobox (combobox :id id :class :style :model model
                                     :listen [:action (fn [e] (reset! (id app-data) (out-fn (value e))))]))]
    [head desc widget]))

(def screen-settings
  (explorer-content
   "Screen settings for the visualization"
   ["<i>Select the screen properties for the visualization</i>."
    "<i>Don't forget pressing enter after entering a new value in the text field.</i>"]
   (grid-panel
    :columns 3
    :hgap 5 :vgap 15
    :items
    (flatten
     (reduce (fn [a b] (conj a (apply create-widget b))) []
             [[:width "This specifies the applet screen width" :text #(Integer/valueOf %)]
              [:height "This specifies the applet screen height" :text #(Integer/valueOf %)]
              [:filter "This sets a lower bound of tweets for the considered languages"
               :combobox #(Integer/valueOf %) 0 10 25 50 100]
              [:loc? "Display the coordenates of the mouse"
               :combobox boolean "true" "false"]])))))

(def viz-mode
  (explorer-content
   "Mode of the visualization"
   ["<i>Select between <b>bilingual</b> to compare languages agains each other or <b>multilingual</b>
    to visualize total number of languages per region</i>."
    "<i>There is also the possibility to select different aggregation metrics.</i>"]
   (grid-panel
    :columns 3
    :hgap 5 :vgap 15
    :items
    (flatten
     (reduce (fn [a b] (conj a (apply create-widget b))) []
             [[:mode "Multilingual" :combobox keyword "bilingual" "multiligual"]
              [:mode-fn "Entropy or count (only affects the multilingual mode"
               :combobox str "entropy" "count"]])))))

(def explorer-settings
  {"Grid files" grid-files
   "Init menu" init-menu
   "Screen settings" screen-settings
   "Visualization mode" viz-mode})

(defn explore []
  (border-panel
   :hgap 5 :vgap 5 :border 5
   :north (label-x :wrap-lines? true
                   :text "Select your settings for visualization")
   :center (left-right-split
            (scrollable
             (listbox-x :id :leftmenu
                        :model (keys explorer-settings)
                        :highlighters [((hl-color :background :lightblue) :rollover-row)]))
            (border-panel :id :rightmenu
                          :center (get explorer-settings "Grid files"))
            :divider-location 1/4)))

(defn create []
  (border-panel))

(defn make-frame []
  (frame :title "TwitViz"
         :size [640 :by 480]
         :content
         (tabbed-panel
          :tabs [{:title "Explore Grid" :content (explore)}
                 {:title "Create Grid"  :content (create)}])))

(defn add-listeners [root]
  (let [leftmenu   (select root [:#leftmenu])
        rightmenu  (select root [:#rightmenu])
        files-table (select root [:#files-table])
        descriptions (select root [:#descriptions])]
    (listen leftmenu :selection
            (fn [e]
              (replace! rightmenu
                        (select rightmenu [:#explorer-content])
                        (explorer-settings (selection leftmenu)))))
    (listen files-table :selection
            (fn [e]
              (let [newval (:file (value-at files-table (selection files-table)))]
                (reset! (:grid app-data) newval)
                (text! descriptions (get-description newval))))))
  root)

(defn run-menu []
  (-> (make-frame) add-listeners ;pack!
      show!))

(run-menu)

;;                      [(label-x :text "Width" :wrap-lines? true :font "ARIAL-BOLD-15")
;;                       (label-x :text "This specifies the applet screen width" :wrap-lines? true)
;;                       (text :id :width :columns 3 :text (str @(:width app-data))
;;                             :listen [:action (fn [e] (reset! (:width app-data) (Integer/valueOf (text e))))])

;; ;                      (button :text "AAA" :listen [:action (fn [e] (alert (str @(:width app-data))))])
;;                       (label-x :text "Height" :wrap-lines? true :font "ARIAL-BOLD-15")
;;                       (label-x :text "This specifies the applet screen height" :wrap-lines? true)
;;                       (text :id :width :columns 3 :text (str @(:height app-data))
;;                             :listen [:action (fn [e] (reset! (:height app-data) (Integer/valueOf (text e))))])

;;                       (label-x :text "Minimum tweets per language" :wrap-lines? true :font "ARIAL-BOLD-15")
;;                       (label-x :text "This sets a lower bound of tweets for the considered languages"
;;                                :wrap-lines? true)
;;                       (combobox :id :filter :class :style :model [0 10 25 50 100]
;;                                 :listen [:action (fn [e] (reset! (:filter app-data) (value e)))])

;;                       (label-x :text "Draw location?" :font "ARIAL-BOLD-15")
;;                       (label-x :text "Display the coordenates of the mouse" :wrap-lines? true)
;;                       (combobox :id :loc? :class :style :model ["true" "false"]
;;                                 :listen [:action (fn [e] (reset! (:loc? app-data) (value e)))])]
