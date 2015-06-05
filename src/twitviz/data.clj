(ns twitviz.data
  (require [clojure.string :as s]))

(def centers
  {:berlin [52.516042 13.390245]
   :amsterdam [52.370292 4.900077] 
   :antwerp [51.220763 4.401598] 
   :brussels [50.844625 4.352359]})

(def boxes
  {:berlin  [52.33963 13.089155 52.675454 13.761118]
   :amsterdam [52.327927 4.789967 52.426971 4.976362]
   :antwerp [51.113175 4.245066 51.323395 4.611823]
   :brussels [50.745668 4.208164 50.942552 4.562496]})

(def bots
  {:berlin [112169930, 1212442812, 1336218432, 1597521211,
            160874621, 161262801, 186899860, 288715859,
            71528370, 81237494, 2309807226, 343197788,
            352734759, 422055979, 436016601, 456864067]})

(def grid-files ["amsterdam_1000_all.grid" "amsterdam_1000_day.grid" "amsterdam_1000_night.grid"
                 "amsterdam_100_all.grid" "amsterdam_100_day.grid" "amsterdam_100_night.grid"
                 "amsterdam_50_all.grid" "amsterdam_50_day.grid" "amsterdam_50_night.grid"
                 "antwerp.grid" "antwerp_1000_all.grid" "antwerp_1000_day.grid" "antwerp_1000_night.grid"
                 "antwerp_100_all.grid" "antwerp_100_day.grid" "antwerp_100_night.grid" "antwerp_50_all.grid"
                 "antwerp_50_day.grid" "antwerp_50_night.grid" "berlin.grid" "berlin_50_all.grid"
                 "berlin_50_day.grid" "berlin_50_night.grid" "brussels_1000_all.grid" "brussels_1000_day.grid"
                 "brussels_1000_night.grid" "brussels_100_all.grid" "brussels_100_day.grid"
                 "brussels_100_night.grid" "brussels_50_all.grid" "brussels_50_day.grid"
                 "brussels_50_night.grid"])

(def grids (map #(str "files/" %) grid-files))

(defn generate-description [fname]
  (let [parts (-> fname
                  (s/split #"\.") first
                  (s/split #"/") last
                  (s/split #"_"))
        three (fn [city c mode]
                (case mode
                  "all" (str (s/capitalize city) " day&night.\nIncludes at most " c " tweets per user.")
                  "day" (str (s/capitalize city) " during the day.\nFor simplification purposes, day period is defined as from 7:00 to 23:00 across the entire day.\nIncludes at most " c " tweets per user.")
                  "night" (str (s/capitalize city) " by night.\nFor simplification purposes, night period is defined as between 23:00 and 7:00 across the entire day.\nIncludes at most " c " tweets per user.")
                  :default "No given description."))
        four (fn [city c mode _] (str (three city c mode) "\nBased on a bot-clean sample of the TwitViz dataset."))]
    (cond
      (= (count parts) 4) (apply four parts)
      (= (count parts) 3) (apply three parts)
      (= (count parts) 1) (str "Entire raw " (s/capitalize (first parts)) " TwitViz dataset.\nNote that no cleaning was attempted for this dataset.")
      :else "No given description.")))

(defn- get-description [fname]
  (if (not (some #{fname} grids)) "No given description."
      (generate-description fname)))

(def get-description (memoize get-description))
