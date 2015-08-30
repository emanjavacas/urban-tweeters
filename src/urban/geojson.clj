#!/bin/bash lein-exec

(use '[leiningen.exec :only (deps)])
(deps ;'[urban.make-grid :refer [pps->geojson tweet-stream tweet->lang]]
         ;'[urban.utils :refer [tweet->coors]]
 '[[clj-json "0.5.3"]
   [org.clojure/data.json "0.2.1"]])

(require '[clojure.data.json :refer [read-str write-str]]
         '[clj-json.core :refer [parse-string]])

;;; utils
(defn lazy-lines [in-fn & {:keys [input] :or {input :file}}]
  (letfn [(helper [rdr]
            (lazy-seq (if-let [line (.readLine rdr)]
                        (cons line (helper rdr))
                        (do (.close rdr) nil))))]
    (case input
      :file (helper (clojure.java.io/reader in-fn))
      :dir (let [fs (.listFiles (clojure.java.io/file in-fn))]
             (flatten (map #(helper (clojure.java.io/reader %)) fs))))))

(defn crossing-number
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
  "point inside an irregular polygon?"
  [p poly]
  (odd?
   (reduce +
           (map #(crossing-number p %)
                (concat (partition 2 1 poly)
                        (list (list (last poly) (first poly))))))))

(defn tweet->coors
  "extract coors and invert their order to catch up with twitter"
  [t]
  (let [[y x] (get-in t ["coordinates" "coordinates"])]
    [x y]))


;;; make-grid
(defn tweet-stream
  "fast json parsing for tweets"
  [in-fn & ids]
  (let [tweets (map parse-string (lazy-lines in-fn))]
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

(defn in-feature? [p feat]
  (let [feattype (get-in feat ["geometry" "type"])
        coords   (get-in feat ["geometry" "coordinates"])]
    (case feattype
      "Polygon"      (inside? p (first coords))
      "MultiPolygon" (some #(inside? p %) (first coords)))))

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

;;; script
(defn prn-err [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn ->pts [pts-fn]
  (for [tw (tweet-stream pts-fn)
        :when (tweet->lang tw :majority)]
    ((juxt tweet->coors #(tweet->lang % :majority)) tw)))

(defn main [geo-fn pts-fn]
  (let [ps (->pts pts-fn)
        geojson (read-str (slurp geo-fn))
        result-map (pps->geojson geojson ps)]
    (do (println (write-str result-map :escape-unicode false))
        (System/exit 0))))

(defn handle-args [& args]
  (if (not (= 3 (count args)))
    (prn-err "wrong number of arguments: " (count args) "\n"
             "Usage:" (first args) "input_geojson" "input_points")
    (let [[_ geo-fn pts-fn] args]
      (main geo-fn pts-fn))))

(apply handle-args *command-line-args*)

