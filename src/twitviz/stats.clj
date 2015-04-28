(ns twitviz.stats
  (:require [twitviz.utils :as u]
            [incanter core stats charts]))

(defn map-kv
  "apply function f over the vals of m.
  returns a map with the new vals"
  [m f]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(def berlin-hoods (u/fetch-hoods "resources/hoods/berlin.geojson.mod"))
(def berlin-hoods-sim (u/fetch-hoods "resources/hoods/berlin.json"))
(def amsterdam-hoods (u/fetch-hoods "resources/hoods/amsterdam.geojson"))
(def brussels-hoods (u/fetch-hoods "resources/hoods/bruxelles.json"))
(def antwerp-hoods (u/fetch-hoods "resources/hoods/antwerp.geojson.mod"))
(def ant-grid (u/load-grid "resources/antwerp.grid"))
(def ber-grid (u/load-grid "resources/berlin.grid"))


;;; M = number of groups
;;; Gm = number of items of group m
;;; N = number of items

;;; ps : [p1 p2 ... pm] = [G1/N G2/N ... GM/N]
(defn entropy-score [ps]
  (reduce + (map (fn [p] (* p (Math/log (/ 1 p)))) ps)))

(defn gs->ps
  ([gs] (gs->ps gs (reduce + gs)))
  ([gs total]
   (map #(/ % total) gs)))

;; ;;; gms : [[g11 g12 .. g1m]
;; ;;         [g21 g22 .. g2m]
;; ;;;          .   .      .
;; ;;;        [gt1 gt2 .. gtm]]
;; ;;; gms (sparse) : 
;; ;;; 
;; ;;; t : organizational unit
;; ;;; m : group
;; (defn entropy-index [gms]
;;   (let [T (reduce + (mapcat vals gms))
;;         gs (into #{} (mapcat keys gms))
;;         ps (map-kv #(/ % T) (merge-with + gms))
;;         pms (map (fn [m] (map-kv m #(/ % (reduce + (vals m))))))]
;;     (reduce + )))

(defn entropy-index [gms]
  (let [T (reduce + (mapcat vals gms))
        E (entropy-score (gs->ps (mapcat vals gms) T))
        ET (* E T)]
    (reduce + (map (fn [m]
                     (let [t (reduce + (vals m))
                           e (entropy-score (gs->ps (vals m) t))]
                       (/ (* t (- E e))
                          ET)))
                   gms))))

(entropy-index (map second ber-grid))

(def gms (map second grid))
(def T (reduce + (mapcat vals gms)))
(def ps (gs->ps (mapcat vals gms) T))
(def E (entropy-score ps))
(def ET (* E T))

