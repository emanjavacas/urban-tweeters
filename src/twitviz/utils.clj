(ns twitviz.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [my-utils.syntax :refer :all]
            [my-utils.io :refer [lazy-lines parse-number]]
            [clojure.data.json :as json]
            [clj-json.core :as clj-json]))

;;; CONSTANTS
(def in-f "/Users/quique/data/tweets.json")
(def in-dir "/Users/quique/data/")

(def centers
  {:berlin [52.516042 13.390245]
   :amsterdam [52.370292 4.900077] 
   :antwerp [51.220763 4.401598] 
   :brussels [50.844625 4.352359] 
   :nyc [40.725913 -73.98672]})

(def boxes
  {:berlin  [52.33963 13.089155 52.675454 13.761118]
   :amsterdam [52.327927 4.789967 52.426971 4.976362]
   :antwerp [51.113175 4.245066 51.323395 4.611823]
   :brussels [50.745668 4.208164 50.942552 4.562496]})

;;; GENERAL
(defn iterator
  "defines a lazy-seq that iterates over
  a given seq. Given dependance on nth and
  count it should be called on vecs"
  ([coll] (iterator coll 0))
  ([coll n]
   (lazy-seq
    (cons (nth coll n)
          (iterator coll (if (= (inc n) (count coll))
                           0 (inc n)))))))

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

(defn tweet-stream [city]
  (map clj-json/parse-string (lazy-tweets (str in-dir "streaming_data/" city ".json"))))


;;; TWEET-MINING
(defn tweet->lang
  "extract language guesses (new-format)"
  [t]
  (let [{l "langs"} t]
    (try
      (all-but-n (vals l))
      (catch Exception e (println t)))))

(defn tweet->coors
  "extract coors and invert their order to catch up with twitter"
  [t]
  (let [[y x] (get-in t ["coordinates" "coordinates"])]
    [x y]))

(defn ps->grid
  "@tiles: [[x y w h] [x y w h] ...]
   @ps: [[lang [x y]] [lang [x y]] ...]
   @return: {[x y] {l1 n l2 m}, ...}"
  ([tiles ps] (ps->grid tiles ps in-rect?))
  ([tiles ps in-fn]
   (reduce (fn [result [l [px py]]]
             (let [tile (first (filter #(in-fn px py %) tiles))
                   nested-d (or (result tile) {})]
               (assoc result tile (update-in nested-d [l] (fnil inc 0)))))
           {} ps)))

(defn ps->grid2
  "@tiles: [[x y side] [x y side] ...]
   @ps: [[lang [x y]] [lang [x y]] ...]
   @return: {[x y] {l1 n l2 m}, ...}"
  ([tiles ps] (ps->grid tiles ps in-rect?))
  ([tiles ps in-fn]
   (let [aux
         (fn [xs]
           (let [result (atom {})]
             (doseq [[l [px py]] xs]
               (let [tile (first (filter #(in-fn px py %) tiles))]
                 (if (get-in @result [tile l])
                   (swap! result update-in [tile l] (partial inc))
                   (swap! result assoc-in [tile l] 1))))
             @result))]
     (reduce (fn [a b] (deep-merge-with + a b))
             (pmap aux (partition-all 2000 ps))))))

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
(defn create-grid
  "computes a grid with nm^2 tiles"
  [city nm]
  (let [[min-lat min-lon max-lat max-lon] (boxes (keyword city))
        w (/ (- max-lat min-lat) nm)
        h (/ (- max-lon min-lon) nm)
        tiles (for [lat (range min-lat max-lat w)
                    lon (range min-lon max-lon w)]
                [lat lon w])
        ps (for [tw (tweet-stream city)]
             [(tweet->lang tw) (tweet->coors tw)])]
    (ps->grid2 tiles (filter #(not (nil? (first %))) ps))))

(defn fetch-ls
  "returns a set of language from a grid"
  ([grid] (fetch-ls grid 20))
  ([grid min-count]
   (conj (keys (filter (fn [[k v]]
                         (> v min-count))
                       (apply merge-with + (vals grid))))
         "UN")))

(defn save-grid [grid fname]
  (with-open [wrt (clojure.java.io/writer (str fname ".grid"))]
    (binding [*out* wrt]
      (let [[lat lon side] (first (second grid))]
        (println side))
      (doseq [[tile ls] grid
              :when tile
              :let [[lat lon side] tile
                   prt-tile (str lat " " lon)
                   prt-ls (apply str (interpose " " (flatten (seq ls))))]]
          (println (str prt-tile " | " prt-ls))))))

(defn load-grid [grid-fn]
  (let [grid (atom {})
        lines (lazy-lines grid-fn)
        side (parse-number (first lines))
        pts (rest lines)]
    (doseq [pt pts
            :let [[tile ls] (map s/trim
                                 (s/split pt #" \| "))
                  [lon lat] (map parse-number (s/split tile #" "))
                  ls-map (-> (apply hash-map (s/split ls #" "))                             
                             (map-kv parse-number))]]
      (swap! grid assoc [lon lat side] ls-map))
    @grid))



;;; HOODS
(defn fetch-hoods [cityfn]
  (let [cityjson (json/read-json (slurp cityfn))]
    (map #(first (get-in % [:geometry :coordinates])) (:features cityjson))))


;; Map of lang to tweets
;; (def by-lang (group-by tweet->lang (tweet-stream "amsterdam")))
;;; counts of distinct users by 
;; (count (group-by #(get-in % ["user" "id"]) (get by-lang "de")))
;; (def by-id (group-by #(get-in % ["user" "id"]) (tweet-stream "amsterdam")))
;; (for [t (get by-lang "is")]
;;   (get-in t ["coordinates" "coordinates"]))

;; (defn inside?
;;   "Is point inside the given polygon?"
;;   [p poly]
;;   (odd?
;;    (reduce + (for [n (range (dec (count poly)))]
;;                (crossing-number p [(nth poly n)
;;                                    (nth poly (+ n 1))])))))
