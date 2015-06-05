(ns twitviz.make-grid
  (:require [clojure.data.json :as json]
            [clj-json.core :as clj-json];fast json reader
            [clj-time.core :as time]
            [clj-time.format :as format]
            [twitviz.data :refer [bots boxes]]
            [twitviz.utils :refer [inside? tweet->coors find-city deep-merge-with]]))

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

(defn tweet->lang
  "extract language guesses"
  [t]
  (let [{l "langs"} t]
    (all-but-n (vals l))))

(defn night?
  "returns boolean, parses twitter date format"
  [date-str start end]
  (let [date (format/parse (format/formatter "EEE MMM dd HH:mm:ss Z yyyy") date-str)
        hour (time/hour date)]
    (or (>= hour start) (<= hour end))))

(defn tweet->night? [t]
  (night? (get t "created_at") 22 7))

(defn fetch-hoods
  "loads a hood"
  [city]
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
