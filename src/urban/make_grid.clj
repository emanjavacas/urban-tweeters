(ns urban.make-grid
  (:require [clojure.data.json :as json]
            [clj-json.core :as clj-json];fast json reader
            [clj-time.core :as time]
            [clj-time.format :as format]
            [com.evocomputing.colors :as colors]
            [urban.data :refer [bots boxes]]
            [urban.utils :refer [inside? tweet->coors find-city deep-merge-with lazy-lines
                                 max-range entropy rescaler load-grid invert]]))

(defn tweet-stream
  "fast json parsing for tweets"
  [in-fn & ids]
  (let [tweets (map clj-json/parse-string (lazy-lines in-fn))]
    (if (not ids) tweets (filter #(not (apply some #{(get-in % ["user" "id"])} ids)) tweets))))

(defn all-but-n
  "return the element ocurring at least k-n times
  where k is the length of coll"
  ([coll] (all-but-n coll 1))
  ([coll m]
   (let [f (frequencies coll)
         v (- (count coll) m)]
     (some #(when (<= v (val %)) (key %)) f))))

;; (defn tweet->lang
;;   "extract language guesses"
;;   [t]
;;   (let [{l "langs"} t]
;;     (all-but-n (vals l))))

(defn majority [xs]
  (let [n (count xs)
        mid (Math/floor (/ n 2))
        f (frequencies xs)
        val (first (sort-by second > f))]
    (if (> (second val) mid)
      (first val)
      nil)))

(defn tweet->lang [tw & format]
  (case (first format)
    :majority (majority
                ((juxt #(get % "langid_guess")
                       #(get % "cld2_guess")
                       #(get % "langdetect_guess"))
                 (get tw "langs")))
    :all-but-1 (all-but-n (vals (get tw "langs")))
    :default   (get tw "lang")))

(defn night?
  "returns boolean, parses twitter date format"
  [date-str start end]
  (let [date (format/parse (format/formatter "EEE MMM dd HH:mm:ss Z yyyy") date-str)
        hour (time/hour date)]
    (or (>= hour start) (<= hour end))))

(defn tweet->night? [t]
  (night? (get t "created_at") 22 7))

(def example
  {"type" "FeatureCollection"
   "features"
   [{"type" "Feature"
     "properties" {"name" "x"}
     "geometry"   {"type" "MultiPolygon"
                   "coordinates" [[[]]]}
     }
    {"type" "Feature"
     "properties" {"name" "y"}
     "geometry"   {"type" "MultiPolygon"
                   "coordinates" [[[]]]}
     }
    {"type" "Feature"
     "properties" {"name" "z"}
     "geometry"   {"type" "MultiPolygon"
                   "coordinates" [[[]]]}
     }
    ]
   })

(defn in-feature [p feat]
  (let [feattype (get-in feat ["geometry" "type"])
        coords   (get-in feat ["geometry" "coordinates"])]
    (case feattype
      "Polygon"      (inside? [x y] coords)  
      "MultiPolygon" (some #(inside? [x y] %) coords))))

;;; Grid operations
(defn- next-lower
  "assumes array[0] > x < array[n]"
  [array x]
  (try 
    (let [i (java.util.Arrays/binarySearch array x)]
      (if (neg? i)
        (aget ^doubles array (Math/abs (+ 2 i)))
        (aget ^doubles array i)))
    (catch Exception e (println (str x "  " (type x))))))

(defn- ps->tile
  "returns a sequence of maps {coord : val}"
  [ps xs ys]
  (for [[[x y] v] ps
        :let [px (next-lower xs x)
              py (next-lower ys y)]]
    {[px py] {v 1}}))

(defn- add-wh [grid w h]
  (clojure.set/rename-keys
   grid (zipmap (keys grid)
                (map #(into % [w h]) (keys grid)))))

(defn make-square-grid
  ([[min-x min-y max-x max-y] nm ps]
   (let [w (/ (- max-x min-x) nm)
         h (/ (- max-y min-y) nm)]
     (make-square-grid [min-x min-y max-x max-y] w h ps)))
  ([[min-x min-y max-x max-y] w h ps]
   (let [xs (double-array (range min-x max-x w))
         ys (double-array (range min-y max-y h))
         grid (reduce (partial deep-merge-with +)
                      (ps->tile ps xs ys))]
     (add-wh grid w h))))

(defn ps->geogrid
  "insert points into a geogrid (geojson-formatted polygrid)"
  [geojson ps]
  (let [main-path ["features"]
        transition (atom (update-in geojson ["features"] #(zipmap (range) %)))]
    (doseq [[p v] ps
            [idx feat] (get-in @transition main-path)
            :when (in-feature p feat)
            :let [props-path  (into main-path [idx "properties" "langs" v])]]
     (swap! transition update-in props-path (fnil inc 0)))
    (update-in @transition ["features"] #(vec (vals %)))))

;; (defn- ps->poly
;;   "returns a sequence of maps {poly : val}"
;;   [ps polys]
;;   (for [[coors d] polys
;;         [p v] ps
;;         :let [feattype (get-in d [:feature :type])]
;;         :when (case feattype
;;                 "Polygon" (inside? p coors)
;;                 "MultiPolygon" (some #(inside? p %) coors))]
;;     {coors {v 1}}))

;; (defn make-polygrid
;;   [ps polys]
;;   (reduce (partial deep-merge-with +)
;;           (ps->poly ps polys)))

;; (defn tiles->polys [grid polys]
;;   (reduce #(deep-merge-with + % %2)
;;           (for [[coors m] polys
;;                 [[x y w h] v] grid
;;                 :when (inside? [(+ (/ w 2) x) (+ (/ h 2) y)] coors)]
;;             {coors {[x y w h] v}})))

(defn save-grid-sparse [fname grid]
  (with-open [wrt (clojure.java.io/writer (str fname ".grid"))]
    (binding [*out* wrt]
      (let [[lat lon w h] (first (second grid))]
        (println w)
        (println h))
      (doseq [[tile ls] grid
              :when tile
              :let [[lat lon w h] tile
                   prt-tile (str lat " " lon)
                   prt-ls (apply str (interpose " " (flatten (seq ls))))]]
          (println (str prt-tile " | " prt-ls))))))

(defn lang->color [ls myfn max-val max-sum & {:keys [format]}]
  (let [[a b c d]
        (case format
          :rgb [255
                (int ((rescaler 0 max-val 0 255) (myfn ls)))
                0
                128]
          :hsl [(int ((rescaler 0 max-val 0 144) (myfn ls)))
                100
                (int ((rescaler 0 max-sum 0 100) (reduce + ls)))
                128])]
    (->> [a b c d]
        (apply colors/create-color)
        (colors/rgb-hexstr))))

(defn tile->props 
  [ls max-count max-sum max-entropy]
  {"count-hsl"   (lang->color (vals ls) count max-count max-sum   :format :hsl)
   "entropy-hsl" (lang->color (vals ls) entropy max-entropy max-sum :format :hsl)
   "count-rgb" (lang->color (vals ls) count max-count max-sum :format :rgb)
   "entropy-rgb" (lang->color (vals ls) entropy max-entropy max-sum :format :rgb)
   "langs" ls})

(defn squaregrid->geojson
  "convert grid structure into colorized geojson"
  [grid]
    (let [max-entropy (max-range grid entropy)
          max-count (max-range grid count)
          max-sum (max-range grid #(reduce + %))
          feats (for [[dims ls] grid 
                      :let [props (tile->props ls max-count max-sum max-entropy) 
                            coors (let [[lon lat w h] dims]
                                    [[[lat, lon], [(+ lat h), (+ lon w)]]])
                            geom {"type" "Polygon" "coordinates" coors}]]
                  {"type" "Feature" "properties" props "geometry" geom})]
    {"type" "FeatureCollection" "features" feats}))

(defn colorize-geojson
  "colorize a geogrid (geojson-formatted polygrid)"
  [geogrid]
  (let [grid (map #([(get-in % ["geometry" "coordinates"])
                     (get-in % ["properties" "langs"])])
                  (get geogrid "features"))
        max-entropy (max-range grid entropy)
        max-count (max-range grid count)
        max-sum (max-range grid #(reduce + %))
        new-feats (map #(tile->props % max-count max-sum max-entropy) (map second grid))]
    (update-in geogrid ["features"] new-feats)))

;;; Load points
(def ps (for [tw (tweet-stream "/Users/quique/data/twitproj/streaming_data/new/antwerp_100.json")
              :when (tweet->lang tw :majority)]
          ((juxt tweet->coors #(tweet->lang % :majority)) tw)))
;;; Load hoods
;; (def antwerp-hood (fetch-hoods "resources/hoods/antwerp.geojson" [:wijknaam]))
;; ;;; Compute grid
;; (def antwerp-polygrid (make-polygrid (take 1000 ps) antwerp-hood))
;; ;;; Transform into json
;; (def antwerp (load-grid "files/antwerp.grid"))
;; (grid->geojson "test.geojson" antwerp-polygrid :format :poly :extraprops antwerp-hoodprops :hoodprops [:wijknaam])

;; (def antwerp-hoodprops (fetch-hoodprops "resources/hoods/antwerp.geojson" [:wijknaam]))
;; (first antwerp-hoodprops)

;; (defn ps->poly2 [ps polys]
;;   (r/fold
;;    (r/monoid #(deep-merge-with + % %2) hash-map)
;;    (r/map (fn [{:keys [meta geometry]}]
;;             (map (fn [[p l]] {meta {l 1}})
;;                    (filter #(inside? (first %) geometry)
;;                              ps)))
;;           polys)))

;;;;;;;;;
;;; MAKE NIGHT BOT-FREE GRID
;; (def infn (str "/Users/quique/data/streaming_data/berlin_50.json"))
;; (def ps-night (for [tw (tweet-stream infn (:berlin bots))
;;                     :when (and (tweet->lang tw) (tweet->night? tw))]
;;                 ((juxt tweet->coors tweet->lang) tw))) 
;; (def berlin-night (make-square-grid (:berlin boxes) 100 ps-night))
;; (save-grid "berlin_night_clean50" berlin-night)

;;; MAKE DAY BOT-FREE GRID
;; (def ps-day (for [tw (tweet-stream infn (:berlin bots))
;;                   :when (and (tweet->lang tw) (not (tweet->night? tw)))]
;;                 ((juxt tweet->coors tweet->lang) tw))) 
;; (def berlin-day (make-square-grid (:berlin boxes) 100 ps-day))
;; (save-grid "berlin_day_clean50" berlin-day)

;;; MAKE TOTAL BOT-FREE GRID
;; (def ps (for [tw (tweet-stream infn (:berlin bots))
;;               :when (and (tweet->lang tw))]
;;           ((juxt tweet->coors tweet->lang) tw))) 
;; (def berlin (make-square-grid (:berlin boxes) 100 ps))
;; (save-grid "berlin_clean50" berlin)

;; (def infn "/Users/quique/data/tweets/")
;; (doseq [f (map #(.getName %) (.listFiles (clojure.java.io/file infn)))
;;         :when (.endsWith f "json")
;;         :let [f (str infn f)
;;               city (find-city f)
;;               ps-night (for [tw (tweet-stream f)
;;                              :when (and (tweet->lang tw) (tweet->night? tw))]
;;                          ((juxt tweet->coors tweet->lang) tw))
;;               grid-night (make-square-grid ((keyword city) boxes) 100 ps-night)
;;               ps-day (for [tw (tweet-stream f)
;;                            :when (and (tweet->lang tw) (not (tweet->night? tw)))]
;;                        ((juxt tweet->coors tweet->lang) tw))
;;               grid-day (make-square-grid ((keyword city) boxes) 100 ps-day)
;;               ps (for [tw (tweet-stream f)
;;                        :when (and (tweet->lang tw))]
;;                    ((juxt tweet->coors tweet->lang) tw))
;;               grid (make-square-grid ((keyword city) boxes) 100 ps-day)]]
;;   (save-grid (str (last (butlast (clojure.string/split f #"\."))) "_night") grid-night)
;;   (save-grid (str (last (butlast (clojure.string/split f #"\."))) "_day") grid-day)
;;   (save-grid (str (last (butlast (clojure.string/split f #"\."))) "_all") grid))

;; ;;; 
;; by-hours
;;   (frequencies
;;    (map #(time/hour
;;           (format/parse (format/formatter "EEE MMM dd HH:mm:ss Z yyyy") (get % "created_at")))
;;         (tweet-stream "berlin_50"))))
;; (sort-by first by-hours)

;;; RUN HOODS
;; (def berlin (load-grid "resources/berlin.grid"))
;; (def berlin-hoods (fetch-hoods "berlin"))
;; (def berlin-by-polys (tiles->polys berlin berlin-hoods))
;; (frm-save "berlin_by_polys" berlin-by-polys)
;; (def berlin-hoods-light (fetch-hoods "berlin-light"))
;; (def berlin-by-polys-light (tiles->polys berlin berlin-hoods-light))
;; (frm-save "berlin_by_polys_light" berlin-by-polys-light)
