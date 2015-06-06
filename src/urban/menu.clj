(ns urban.menu
  (:import [urban.protocols Monolingualism Bilingualism Multilingualism])
  (:require [seesaw.core :refer :all]
            [seesaw.swingx :refer :all]
            [seesaw.mig :refer [mig-panel]]
            [seesaw.table :refer [value-at]]
            [urban.utils :refer [find-size find-city find-last load-grid fetch-ls max-range max-lang entropy]]
            [urban.data :refer [grids get-description]]
            [quil.core :refer [defsketch]]
            [urban.protocols])
  (:gen-class))

(native!)

(def app-data
  "define init values for the visualization. Given values are default."
  {:width (atom 800) :height (atom 600) :filter (atom 50) :loc? (atom true)
   :protocol (atom :monolingual) :mode (atom :count)
   :gridfn (atom (first grids))})

(defn app->text [app-data]
  (let [to->name {:width "Width" :height "Height" :filter "Minimum tweets per language"
                  :loc? "Show location?" :protocol "Visualization mode"
                  :mode "Aggregating function" :gridfn "Grid file"}
        data-map (zipmap (map #(str "<b>" (% to->name) "</b>") (keys app-data)) (map deref (vals app-data)))]
    (apply str (flatten ["<html>" (interpose "<br>" (map #(interpose ": " %) data-map))  "<html>"]))))

(defn to->boolean [s]
  (case s
    "true" true
    "false" false))

(defn run-urban-tweeters [{:keys [width height filter loc? protocol mode gridfn]}]
  (let [grid (load-grid @gridfn)
        city (find-city @gridfn)
        ls-idx (fetch-ls grid @filter)
        modes {:entropy {:fn entropy :max (max-range grid entropy)}
               :count   {:fn count   :max (max-range grid count)}}
        max-ls (zipmap ls-idx (map #(max-lang grid %) ls-idx))
        gstate1 {:lang (atom (first ls-idx)) :alpha (atom 50) :map (atom nil) :beta (atom 2) :red (atom 255)}        
        gstate2 {:lang1 (atom (first ls-idx)) :lang2 (atom (second ls-idx)) :red (atom 255)
                 :alpha (atom 50) :beta (atom 0.03) :map (atom nil)}
        gstate3 {:mode mode :alpha (atom 50) :beta (atom 0.03) :map (atom nil) :red (atom 255)}
        monolingual  (Monolingualism.  grid city @width @height @loc? gstate1 ls-idx max-ls)
        bilingual    (Bilingualism.    grid city @width @height @loc? gstate2 ls-idx)
        multilingual (Multilingualism. grid city @width @height @loc? gstate3 modes)
        target     (case @protocol
                     :monolingual monolingual 
                     :bilingual   bilingual 
                     :multilingual multilingual)]
    (defsketch Urban-Tweeters
      :title (str "Displaying " city)
      :setup (urban.protocols/make-setup target)
      :draw (urban.protocols/make-draw target)
      :size [@width @height]
      :renderer :p2d)))

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
    :id :init-menu
    :items [(button
             :text "Run!"
             :mnemonic \R
             :halign :leading :valign :center
             :selected? false
             :listen [:action (fn [e] (run-urban-tweeters app-data))])
            :separator
            (header
             :title "Selected settings"
             :id :selected-settings
             :description (app->text app-data))])))

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
    :model [:columns [:city :file :size :last]
            :rows  (map (fn [g]
                          (zipmap [:city :file :size :last]
                                  ((juxt find-city identity find-size find-last) g)))
                        grids)]
    :horizontal-scroll-enabled? true
    :selection-mode :single)))

(def input-file
  (border-panel
   :west (label-x :text "Insert path to grid file" :wrap-lines? true)
   :center (text :text "" :listen [:action (fn [e] (reset! (:gridfn app-data) (text e)))])))

(def grid-files
  (explorer-content
   "Available Grid files"
   ["<i>A description of the selected grid file is being shown below</i>."
    "<i>The selected file will be used for the visualization after initializing it from the <b>Init menu</b>"]
   (vertical-panel
    :items [files-table :separator descriptions ;input-file
            ])))

(defn create-component [id desc-text component-type parse-fn & model]
  (let [head   [(label :text (apply str (rest (str id))) :font "ARIAL-BOLD-15") "gap 10"]
        desc   [(label :text desc-text :font "ARIAL-ITALIC-14") "gap 10, wrap"]
        sep    [:separator "growx, wrap, gapleft 10, span 2"]
        component (case component-type
                    :text [(text :id id :listen [:action (fn [e] (reset! (id app-data) (parse-fn (text e))))]
                                 :columns 3 :font "ARIAL-ITALIC-12" :text (str @(id app-data))) "gapleft 30, wrap"]
                    :combobox [(combobox :listen [:action (fn [e] (reset! (id app-data) (parse-fn (value e))))]
                                         :id id :class :style :model model) "gapleft 30, wrap"])]    
    [head component desc sep]))

(def screen-settings
  (explorer-content
   "Screen settings for the visualization"
   ["<i>Select the screen properties for the visualization</i>."
    "<i>Don't forget pressing enter after entering a new value in the text field.</i>"]
  (mig-panel :constraints ["" "[left]"]
    :items
    (into []
          (mapcat #(apply create-component %)
           [[:width "This option specifies the applet screen width"
             :text #(Integer/valueOf %)]
            [:height "This option specifies the applet screen height"
             :text #(Integer/valueOf %)]
            [:loc? "Display the coordinates of the mouse"
             :combobox #(Integer/valueOf %)  0 10 25 50 100]
            [:filter "This option sets a lower bound of tweets for the considered languages"
             :combobox to->boolean "true" "false"]])))))

(def viz-mode
  (explorer-content
   "Mode of the visualization"
   ["<i>Select between <b>bilingual</b> to compare languages agains each other or <b>multilingual</b>
    to visualize total number of languages per region</i>."
    "<i>There is also the possibility to select different aggregation metrics.</i>"]
   (mig-panel :constraints ["" "[left]"]
    :items
    (into []
          (mapcat #(apply create-component %)
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
                        :preferred-size [50 :by 10]
                        :model (keys explorer-settings)
                        :highlighters [((hl-color :background :lightblue) :rollover-row)]))
            (border-panel :id :rightmenu
                          :center (get explorer-settings "Grid files"))
            :divider-location 1/5)))

(defn create []
  (border-panel
   :hgap 40 :vgap 40
   :center (label-x :text "Under construction")))

(defn make-frame []
  (frame :title "Urban Tweeters"
         :size [840 :by 500]
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
              (let [new-right (explorer-settings (selection leftmenu))]
                (if-let [selected-settings (select new-right [:#selected-settings])]
                  (config! selected-settings :description (app->text app-data)))
                (replace! rightmenu
                          (select rightmenu [:#explorer-content])
                          new-right))))
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
;; (run-menu)
