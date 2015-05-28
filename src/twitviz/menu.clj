(ns twitviz.menu
  (:import [twitviz.protocols Monolingualism Bilingualism Multilingualism])
  (:require [seesaw.core :refer :all]
            [seesaw.swingx :refer :all]
            [seesaw.table :refer [value-at]]
            [twitviz.utils :refer [find-size find-city find-last load-grid fetch-ls max-range max-lang entropy]]
            [twitviz.data :refer [grids get-description]]
            [quil.core :refer [defsketch]]
            [twitviz.protocols])
  (:gen-class))

(def app-data
  "define init values for the visualization. Given values are default."
  {:width (atom 800) :height (atom 600) :filter (atom 50) :loc? (atom true)
   :protocol (atom :multilingual) :mode (atom :count)
   :gridfn (atom (first grids))})

(defn run-twitviz [{:keys [width height filter loc? protocol mode gridfn]}]
  (let [grid (load-grid @gridfn)
        city (find-city @gridfn)
        ls-idx (fetch-ls grid @filter)
        modes {:entropy {:fn entropy :max (max-range grid entropy)}
               :count   {:fn count   :max (max-range grid count)}}
        max-ls (zipmap ls-idx (map #(max-lang grid %) ls-idx))
        gstate1 {:lang (atom (first ls-idx)) :alpha (atom 50) :map (atom nil)}        
        gstate2 {:lang1 (atom (first ls-idx)) :lang2 (atom (second ls-idx))
                 :alpha (atom 50) :beta (atom 0.03) :map (atom nil)}
        gstate3 {:mode mode :alpha (atom 50) :beta (atom 0.03) :map (atom nil)}
        monolingual  (Monolingualism.  grid city 640 480 @loc? gstate1 ls-idx max-ls)
        bilingual    (Bilingualism.    grid city 640 480 @loc? gstate2 ls-idx)
        multilingual (Multilingualism. grid city 700 600 @loc? gstate3 modes)
        target     (case @protocol
                     :monolingual monolingual 
                     :bilingual   bilingual 
                     :multilingual multilingual)]
    (println "INIT!")
    (defsketch TwitViz
      :title (str "Displaying " city)
      :setup (twitviz.protocols/make-setup target)
      :draw (twitviz.protocols/make-draw target)
      :size [@width @height]
      :renderer :opengl)))

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
             :listen [:action (fn [e] (run-twitviz app-data))])])))

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
            :rows  (map (fn [g]
                          (zipmap [:city :file :size :last]
                                  ((juxt find-city identity find-size find-last) g)))
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

(defn create-widget [id desc-text widget-type parse-fn & model]
  (let [head (label-x :text (apply str (rest (str id))) :wrap-lines? true :font "ARIAL-BOLD-15")
        desc (label-x :text desc-text :wrap-lines? true)
        widget (case widget-type
                 :text (text :id id :columns 3 :text (str @(id app-data))
                             :listen [:action (fn [e] (reset! (id app-data) (parse-fn (text e))))])
                 :combobox (combobox :id id :class :style :model model
                                     :listen [:action (fn [e] (reset! (id app-data) (parse-fn (value e))))]))]
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
             [[:protocol "Select linguistic setup" :combobox keyword  "monolingual" "bilingual" "multilingual"]
              [:mode "Entropy or count (only affects the multilingual mode)"
               :combobox keyword "entropy" "count"]])))))

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
         (border-panel
          :north (toolbar :items [(action :handler dispose! :name "Exit")])
          :center
          (tabbed-panel
           :tabs [{:title "Explore Grid" :content (explore)}
                  {:title "Create Grid"  :content (create)}]))))

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
                (reset! (:gridfn app-data) newval)
                (text! descriptions (get-description newval))))))
  root)

(defn run-menu []
  (-> (make-frame) add-listeners ;pack!
      show!))

(defn -main [& args]
  (run-menu))
