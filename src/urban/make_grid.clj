(ns urban.make-grid
  (:require [clojure.data.json :as json]
            [clj-json.core :as clj-json];fast json reader
            [clj-time.core :as time]
            [clj-time.format :as format]
            [com.evocomputing.colors :as colors]
            [urban.data :refer [bots boxes]]
            [urban.utils :refer [inside? tweet->coors deep-merge-with lazy-lines
                                 max-range entropy rescaler load-grid]]))

(defn tweet-stream
  "fast json parsing for tweets"
  [in-fn & ids]
  (let [tweets (map clj-json/parse-string (lazy-lines in-fn))]
    (if (not ids)
      tweets
      (filter #(not (apply some #{(get-in % ["user" "id"])} ids)) tweets))))

(defn all-but-n
  "return the element ocurring at least k-n times
  where k is the length of coll"
  ([coll] (all-but-n coll 1))
  ([coll m]
   (let [f (frequencies coll)
         v (- (count coll) m)]
     (some #(when (<= v (val %)) (key %)) f))))

(defn majority [xs]
  (let [n (count xs)
        mid (Math/floor (/ n 2))
        f (frequencies xs)
        val (first (sort-by second > f))]
    (if (> (second val) mid)
      (first val)
      nil)))

(defn tweet->lang
  "return language based on a number of guesses following
  diferent strategies, if no format is passed it assumes
  default twitter format and tries to return Twitter lang"
  [tw & format]
  (case (first format)
    :majority (majority
                ((juxt #(get % "langid_guess")
                       #(get % "cld2_guess")
                       #(get % "langdetect_guess"))
                 (get tw "langs")))
    :all-but-1 (all-but-n (vals (get tw "langs")))
    :default   (get tw "lang" "unk")))

(defn night?
  "returns boolean, parses twitter date format"
  [date-str start end]
  (let [date (format/parse (format/formatter "EEE MMM dd HH:mm:ss Z yyyy") date-str)
        hour (time/hour date)]
    (or (>= hour start) (<= hour end))))

(defn tweet->night?
  "was tweet issued at night?"
  [t]
  (night? (get t "created_at") 22 7))

(defn in-feature? [p feat]
  (let [feattype (get-in feat ["geometry" "type"])
        coords   (get-in feat ["geometry" "coordinates"])]
    (case feattype
      "Polygon"      (inside? p (first coords))
      "MultiPolygon" (some #(inside? p %) (first coords)))))

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

(defn ps->geojson
  "insert points into a geojson in a functional fashion"
  [geojson ps]
  (let [features (get geojson "features")
        points
        (for [feat features
              [[y x] v] ps
              :when (in-feature? [x y] feat)
              :let [name (get-in feat ["properties" "Name"])]]  [name v])
        freqs (frequencies points)]
    (reduce (fn [acc [[k1 k2] v]]
              (assoc-in acc [k1 k2] v))
            {}
            freqs)))

(defn pps->geojson
  "insert points into a geojson in a functional parallel fashion"
  [geojson ps]
  (let [features (get geojson "features")
        points
        (mapcat
         (fn [feat]
           (filter identity
                   (pmap
                    (fn [[[y x] v]]
                      (when (in-feature? [x y] feat)
                        [(get-in feat ["properties" "Name"]) v]))
                    ps)))
         features)
        freqs (frequencies points)]
    (reduce (fn [acc [[k1 k2] v]]
              (assoc-in acc [k1 k2] v))
            {}
            freqs)))

(defn- ps->poly
  "returns a sequence of maps {poly : val}"
  [ps polys]
  (for [[coors d] polys
        [p v] ps
        :let [feattype (get-in d [:feature :type])]
        :when (case feattype
                "Polygon" (inside? p coors)
                "MultiPolygon" (some #(inside? p %) coors))]
    {coors {v 1}}))

(defn make-polygrid
  [ps polys]
  (reduce (partial deep-merge-with +)
          (ps->poly ps polys)))

(defn tiles->polys [grid polys]
  (reduce #(deep-merge-with + % %2)
          (for [[coors m] polys
                [[x y w h] v] grid
                :when (inside? [(+ (/ w 2) x) (+ (/ h 2) y)] coors)]
            {coors {[x y w h] v}})))

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


;; (def lor
;;   (json/read-str
;;    (slurp "/Users/quique/data/twitproj/hoods/LOR/LOR-Prognoseraeume.json")))
;; (def ps (for [tw (tweet-stream "/Users/quique/data/twitproj/streaming_data/new/berlin_100.json")
;;               :when (tweet->lang tw :majority)]
;;           ((juxt tweet->coors #(tweet->lang % :majority)) tw)))
;; (def ps-geojson (time (ps->geojson lor (take 100 ps))))
;; (def pps-geojson (time (pps->geojson lor (take 100 ps))))
