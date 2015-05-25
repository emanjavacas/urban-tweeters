(ns twitviz.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [my-utils.syntax :refer :all]
            [my-utils.io :refer [lazy-lines parse-number frm-save]]
            [clojure.data.json :as json]
            [clj-json.core :as clj-json];fast json reader            
;            [clojure.core.reducers :as r]
            [clj-time.core :as time]
            [clj-time.format :as format]))

;;; CONSTANTS
(def centers
  {:berlin [52.516042 13.390245]
   :amsterdam [52.370292 4.900077] 
   :antwerp [51.220763 4.401598] 
   :brussels [50.844625 4.352359] 
;   :nyc [40.725913 -73.98672]
   })
(second (clojure.string/split (str :a) #":"))
(def boxes
  {:berlin  [52.33963 13.089155 52.675454 13.761118]
   :amsterdam [52.327927 4.789967 52.426971 4.976362]
   :antwerp [51.113175 4.245066 51.323395 4.611823]
   :brussels [50.745668 4.208164 50.942552 4.562496]})

(def bots
  {:berlin [112169930, 1212442812, 1336218432, 1597521211, 160874621, 161262801, 186899860, 288715859,
            71528370, 81237494, 2309807226, 343197788, 352734759, 422055979, 436016601, 456864067]})

(def grids
  {"berlin_night" "resources/berlin_night_clean50.grid"
   "berlin_day" "resources/berlin_day_clean50.grid"
   "berlin" "resources/berlin_clean50.grid"
   "antwerp" "resources/antwerp.grid"})

;;; GENERAL
(defn dekey [k] (second (clojure.string/split (str k) #":")))

(defn all-but-n
  "return the element ocurring at least k-n times
  where k is the length of coll"
  ([coll] (all-but-n coll 1))
  ([coll m]
   (let [f (frequencies coll)
         v (- (count coll) m)]
     (some #(when (<= v (val %)) (key %)) f))))

(defn rescaler
  ;http://stackoverflow.com/questions/5294955/
  "returns a function that scales x to a new range (new-min new-max) 
   It needs to know the original range (min max)"
  [min max new-min new-max]
  (fn [x]
    (+ new-min (/ (* (- new-max new-min)
                     (- x min))
                  (- max min)))))

(defn night? [date-str start end]
  (let [date (format/parse (format/formatter "EEE MMM dd HH:mm:ss Z yyyy") date-str)
        hour (time/hour date)]
    (or (>= hour start) (<= hour end))))

(defn tweet->night? [t]
  (night? (get t "created_at") 22 7))

(defn sigmoid [a] ; doesn't need to compute the max-range
  (fn [x] (/ 1 (+ 1 (Math/exp (- (* a x)))))))

;;; GEOMETRY
(defn in-rect?
  [px py [x y w h]]
  (let [h (or h w)]
    (and (>= px x) (< px (+ x w))
         (>= py y) (< py (+ y h)))))

(defn- crossing-number
  "Determine crossing number for given point and segment of a polygon.
   See http://geomalgorithms.com/a03-_inclusion.html"
  [[px py] [[x1 y1] [x2 y2]]]
  (if (or (and (<= y1 py) (> y2 py))
          (and (> y1 py) (<= y2 py)))
    (let [vt (/ (- py y1) (- y2 y1))]
      (if (< px (+ x1 (* vt (- x2 x1))))
        1 0))
    0))

(defn inside? [p poly]
  (odd?
   (reduce +
           (map #(crossing-number p %)
                (concat (partition 2 1 poly)
                        (list (list (last poly) (first poly))))))))

;;; I/O
(defn lazy-tweets [f] 
  (letfn [(helper [rdr]
            (lazy-seq (if-let [line (.readLine rdr)]
                        (cons line (helper rdr))
                        (do (.close rdr) nil))))]
    (helper (io/reader f))))

(def in-dir "/Users/quique/data/")
(defn tweet-stream [city & ids]
  (let [tweets (map clj-json/parse-string
                    (lazy-tweets (str in-dir "streaming_data/" city ".json")))]
    (if (not ids) tweets (filter #(not (apply some #{(get-in % ["user" "id"])} ids)) tweets))))

(defn x [a & b]
  (class b))

;;; TWEET-MINING
(defn tweet->lang
  "extract language guesses"
  [t]
  (let [{l "langs"} t]
    (all-but-n (vals l))))

(defn tweet->coors
  "extract coors and invert their order to catch up with twitter"
  [t]
  (let [[y x] (get-in t ["coordinates" "coordinates"])]
    [x y]))

(defn keep-langs
  [grid & ls]
  (let [m (atom grid)]
    (doseq [[k v] grid
            out-l (into #{} (remove #(some #{%} ls) (keys v)))]
      (swap! m dissoc-in [k out-l]))
    @m))

(defn remove-langs
  [grid & ls]
  (let [m (atom grid)]
    (doseq [k (keys grid)
            l ls]
      (swap! m dissoc-in [k l]))
    @m))

;;; GRID
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

(defn make-square-grid
  ([[min-x min-y max-x max-y] nm ps]
   (let [w (/ (- max-x min-x) nm)
         h (/ (- max-y min-y) nm)]
     (make-square-grid [min-x min-y max-x max-y] w h ps)))
  ([[min-x min-y max-x max-y] w h ps]
   (let [xs (double-array (range min-x max-x w))
         ys (double-array (range min-y max-y h))]
     (reduce (partial deep-merge-with +)
             (ps->tile ps xs ys)))))

(defn- ps->poly
  "returns a sequence of maps {poly : val}"
  [ps polys]
  (for [{:keys [meta geometry]} polys
        [p v] ps
        :when (inside? p geometry)]
    {meta {v 1}}))

(defn make-polygrid
  [ps polys]
  (reduce (partial deep-merge-with +)
          (ps->poly ps polys)))

;; (defn ps->poly2 [ps polys]
;;   (r/fold
;;    (r/monoid #(deep-merge-with + % %2) hash-map)
;;    (r/map (fn [{:keys [meta geometry]}]
;;             (map (fn [[p l]] {meta {l 1}})
;;                    (filter #(inside? (first %) geometry)
;;                              ps)))
;;           polys)))

(defn tiles->polys [grid polys]
  (reduce #(deep-merge-with + % %2)
          (for [{:keys [meta geometry]} polys
                [[x y w h] v] grid
                :when (inside? [(+ (/ w 2) x) (+ (/ h 2) y)] geometry)]
            {meta {[x y w h] v}})))

(defn save-grid [fname grid]
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

(defn load-grid [grid-fn]
  (let [grid (atom {})
        lines (lazy-lines grid-fn)
        w (parse-number (first lines))
        h (parse-number (second lines))
        pts (drop 2 lines)]
    (doseq [pt pts
            :let [[tile ls] (map s/trim (s/split pt #" \| "))
                  [lon lat] (map parse-number (s/split tile #" "))
                  ls-map (-> (apply hash-map (s/split ls #" "))                             
                             (map-kv parse-number))]]
      (swap! grid assoc [lon lat w h] ls-map))
    @grid))

(defn fetch-grid [s]
  (cond (.exists (clojure.java.io/as-file s)) (load-grid s)
        (get grids s) (load-grid (get grids s))
        :else nil))

(defn fetch-hoods [city]
  (let [cities {"berlin" "resources/hoods/berlin.geojson"
                "berlin-light" "resources/hoods/berlin.json"
                "amsterdam" "resources/hoods/amsterdam.geojson"
                "antwerp" "resources/hoods/antwerp.geojson"
                "brussels" "resources/hoods/bruxelles.geojson"}
        cityjson (json/read-json (slurp (get cities city)))]
    (map (fn [hood] {:meta (get-in hood [:properties :name]) 
                     :geometry (vec (for [[y x] (first (get-in hood [:geometry :coordinates]))]
                                      [x y]))})
         (:features cityjson))))

(defn fetch-ls
  "returns a set of language from a grid"
  ([grid] (fetch-ls grid 20))
  ([grid min-count]
   (let [ls (keys (filter (fn [[k v]]
                         (> v min-count))
                          (apply merge-with + (vals grid))))]
     (conj ls "UN"))))

;;;;;;;;;
;;; MAKE NIGHT BOT-FREE GRID
;; (def ps-night (for [tw (tweet-stream "berlin_50" (:berlin bots))
;;                     :when (and (tweet->lang tw) (tweet->night? tw))]
;;                 ((juxt tweet->coors tweet->lang) tw))) 
;; (def berlin-night (make-square-grid (:berlin boxes) 100 ps-night))
;; (save-grid "berlin_night_clean50" berlin-night)

;;; MAKE DAY BOT-FREE GRID
;; (def ps-day (for [tw (tweet-stream "berlin_50" (:berlin bots))
;;                   :when (and (tweet->lang tw) (not (tweet->night? tw)))]
;;                 ((juxt tweet->coors tweet->lang) tw))) 
;; (def berlin-day (make-square-grid (:berlin boxes) 100 ps-day))
;; (save-grid "berlin_day_clean50" berlin-day)

;;; MAKE TOTAL BOT-FREE GRID
;; (def ps (for [tw (tweet-stream "berlin_50" (:berlin bots))
;;               :when (and (tweet->lang tw))]
;;           ((juxt tweet->coors tweet->lang) tw))) 
;; (def berlin (make-square-grid (:berlin boxes) 100 ps))
;; (save-grid "berlin_clean50" berlin)
;; (def x (load-grid "berlin_clean50.grid"))
;; (first x)
;; (first berlin)
;; ;;; 
;; (def by-hours
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
