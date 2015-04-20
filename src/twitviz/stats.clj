(ns twitviz.stats
  (:require [twitviz.utils :as u]
            [incanter core stats charts]))

(def berlin-hoods (u/fetch-hoods "resources/hoods/berlin.geojson.mod"))
(def berlin-hoods-sim (u/fetch-hoods "resources/hoods/berlin.json"))
(def amsterdam-hoods (u/fetch-hoods "resources/hoods/amsterdam.geojson"))
(def brussels-hoods (u/fetch-hoods "resources/hoods/bruxelles.json"))
(def antwerp-hoods (u/fetch-hoods "resources/hoods/antwerp.geojson.mod"))

(defn dissimilarity )



