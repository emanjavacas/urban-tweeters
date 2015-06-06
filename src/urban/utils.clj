(ns urban.utils
  (:import [de.fhpotsdam.unfolding UnfoldingMap]
           [de.fhpotsdam.unfolding.geo Location]
           [controlP5 DropdownList])
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [quil.core :refer [mouse-x mouse-y fill text]]))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level."
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn map-kv
  "apply function f over the vals of m.
  returns a map with the new vals"
  [m f]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^-?\d+\.?\d*([Ee]\+\d+|[Ee]-\d+|[Ee]\d+)?$" (.trim s))
    (read-string s)))

(defn lazy-lines [in-fn & {:keys [input] :or {input :file}}]
  (letfn [(helper [rdr]
            (lazy-seq (if-let [line (.readLine rdr)]
                        (cons line (helper rdr))
                        (do (.close rdr) nil))))]
    (case input
      :file (helper (io/reader in-fn))
      :dir (let [fs (.listFiles (io/file in-fn))]
             (flatten (map #(helper (io/reader %)) fs))))))


;;; Operations on files
(defn find-city
  "find the city in a filename"
  [fname]
  (first (or (re-find #"(berlin|antwerp|brussels|amsterdam)" fname)
             ["unknown" "unknown"])))

(defn find-size
  "find size of a given file in kilobytes"
  [fname]
  (let [size (.getContentLength (.openConnection (io/resource fname)))
        kbs (float (/ size (* 1024 1)))]
    (format "%.3fK" kbs)))

(defn find-last
  "find last modified date of a file"
  ([fname] (find-last fname "MM/dd/yyyy HH:mm:ss"))
  ([fname format]
   (let [date (java.util.Date. (.getLastModified (.openConnection (io/resource fname))))
         formatter (java.text.SimpleDateFormat. format)]
     (.format formatter date))))

;;; Handling color
(defn max-range
  "extract max val across cells in a grid as computed by f"
  [grid f]
  (let [ls (for [[xyz m] grid
                 :when xyz]
             (vals m))]
    (apply max (map f ls))))

(defn max-lang
  "finds the max-val for a given language"
  [grid l]
  (apply max (map #(get % l 0) (vals grid))))

(defn entropy 
  "input must be a vector of raw (count) frequencies"
  [fs]
  (let [log-2 (Math/log 2)
        tot (reduce + fs)]
    (->> fs
         (map (fn [f]
                (let [rf (/ f tot)]
                  (-> (Math/log rf) 
                      (/ log-2) 
                      (* rf) 
                      Math/abs))))
         (reduce +))))

(defn sigmoid
  "returns a function that computes sigmoid
  on a new value `x` shifted by parameter `alpha`"
  [alpha]
  (fn [x] (/ 1 (+ 1 (Math/exp (- (* alpha x)))))))

(defn rescaler
  "returns a function that scales x to a new range
  It needs to know the original range (min max)
  http://stackoverflow.com/questions/5294955/"
  [min max new-min new-max]
  (fn [x]
    (+ new-min (/ (* (- new-max new-min)
                     (- x min))
                  (- max min)))))

(defn safe-log
  "computes log of base `base`, handles `x` less than or equal zero"
  ([x] (safe-log Math/E))
  ([base x]
   (cond (zero? x) 0
         (> 0 x)   0
         :else (/ (Math/log x) (Math/log base)))))

(defn inverse [x base]
  (if (zero? base) 0
      (/ 1 (Math/pow base (- x)))))

;;; Geometry
(defn in-rect?
  "inside a rect?"
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

(defn inside?
  "point inside a irregular polygon?"
  [p poly]
  (odd?
   (reduce +
           (map #(crossing-number p %)
                (concat (partition 2 1 poly)
                        (list (list (last poly) (first poly))))))))

;;; Operations on tweets
(defn tweet->coors
  "extract coors and invert their order to catch up with twitter"
  [t]
  (let [[y x] (get-in t ["coordinates" "coordinates"])]
    [x y]))

;; ;;; Operations on grid
;; (defn load-grid
;;   "loads the grid according to the specification 
;;   given by twitviz.make-grid/save-grid"
;;   [grid-fn]
;;   (let [grid (atom {})
;;         lines (lazy-lines grid-fn)
;;         w (parse-number (first lines))
;;         h (parse-number (second lines))
;;         pts (drop 2 lines)]
;;     (doseq [pt pts
;;             :let [[tile ls] (map s/trim (s/split pt #" \| "))
;;                   [lon lat] (map parse-number (s/split tile #" "))
;;                   ls-map (-> (apply hash-map (s/split ls #" "))                             
;;                              (map-kv parse-number))]]
;;       (swap! grid assoc [lon lat w h] ls-map))
;;     @grid))

(defn load-grid
  "loads the grid according to the specification 
  given by urban.make-grid/save-grid"
  [grid-fn]
  (let [grid (atom {})
        lines (s/split (slurp (io/resource grid-fn)) #"\n")
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

(defn fetch-ls
  "returns a set of language from a grid"
  ([grid] (fetch-ls grid 20))
  ([grid min-count]
   (let [ls (keys (filter (fn [[k v]]
                         (> v min-count))
                          (apply merge-with + (vals grid))))]
     ls
;     (conj ls "unk")
     )))

;;; Operations on Unfolding objects
(defn draw-location
  "draw coordinates at cursor"
  [^UnfoldingMap the-map]
  (let [^Location loc (.getLocation the-map (mouse-x) (mouse-y))]
    (fill 0)
    (text (str (.getLat loc) ", " (.getLon loc))
          (mouse-x)
          (mouse-y))))

;;; Operations on ControlP5
(defn set-items
  "updates items of a ddl
  automatically index them if no keys are passed"
  ([^DropdownList ddl vs]
   (set-items ddl vs (range (count vs))))
  ([^DropdownList ddl ks vs]
   (doseq [[k v] (zipmap ks vs)]
     (.addItem ddl k v))))

