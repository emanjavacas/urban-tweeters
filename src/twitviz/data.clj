(ns twitviz.data)

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

;; (def grids
;;   (map #(str (.getName %))
;;        (.listFiles (clojure.java.io/file (.getFile (clojure.java.io/resource "files/"))))))

(def grid-files ["amsterdam_1000_all.grid" "amsterdam_1000_day.grid" "amsterdam_1000_night.grid"
                 "amsterdam_100_all.grid" "amsterdam_100_day.grid" "amsterdam_100_night.grid"
                 "amsterdam_50_all.grid" "amsterdam_50_day.grid" "amsterdam_50_night.grid"
                 "antwerp.grid" "antwerp_1000_all.grid" "antwerp_1000_day.grid" "antwerp_1000_night.grid"
                 "antwerp_100_all.grid" "antwerp_100_day.grid" "antwerp_100_night.grid" "antwerp_50_all.grid"
                 "antwerp_50_day.grid" "antwerp_50_night.grid" "berlin.grid" "berlin_50_all.grid"
                 "berlin_50_day.grid" "berlin_50_night.grid" "berlin_clean50.grid" "berlin_day_clean50.grid"
                 "berlin_night_clean50.grid" "brussels_1000_all.grid" "brussels_1000_day.grid"
                 "brussels_1000_night.grid" "brussels_100_all.grid" "brussels_100_day.grid"
                 "brussels_100_night.grid" "brussels_50_all.grid" "brussels_50_day.grid"
                 "brussels_50_night.grid"])

(def grids (map #(str "files/" %) grid-files))

(def descs
  {"files/berlin_night_clean50.grid"
   "Berlin by night. Night period is defined as between 23:00 and 7:00 across the entire year.\nBased on a clean sample of the entire TwitViz Berlin set.\nAt most 50 tweets per user were taken into account in order to prevent overprolific users from distorting the resulting distribution."
   "files/berlin_day_clean50.grid" 
   "Berlin during the day. Day period is defined as from 7:00 to 23:00 across the entire year.\nBased on a clean sample of the entire TwitViz Berlin set.\nIncludes at most 50 tweets per user."
   "files/berlin_clean50.grid" 
   "Berlin day&night.\nBased on a clean sample of the entire TwitViz Berlin set.\nIncludes at most 50 tweets per user."
   "files/berlin.grid" "Entire TwitViz Berlin dataset.\nNote that no cleaning was attempted for this dataset."
   "files/antwerp.grid" "Entire TwitViz Antwerp dataset.\nNote that no cleaning was attempted for this dataset."})

;; (def descs
;;   {"resources/berlin_night_clean50.grid"
;;    "Berlin by night. Night period is defined as between 23:00 and 7:00 across the entire year.\nBased on a clean sample of the entire TwitViz Berlin set.\nAt most 50 tweets per user were taken into account in order to prevent overprolific users from distorting the resulting distribution."
;;    "resources/berlin_day_clean50.grid" 
;;    "Berlin during the day. Day period is defined as from 7:00 to 23:00 across the entire year.\nBased on a clean sample of the entire TwitViz Berlin set.\nIncludes at most 50 tweets per user."
;;    "resources/berlin_clean50.grid" 
;;    "Berlin day&night.\nBased on a clean sample of the entire TwitViz Berlin set.\nIncludes at most 50 tweets per user."
;;    "resources/berlin.grid" "Entire TwitViz Berlin dataset.\nNote that no cleaning was attempted for this dataset."
;;    "resources/antwerp.grid" "Entire TwitViz Antwerp dataset.\nNote that no cleaning was attempted for this dataset."})

(defn get-description [g]
  (get descs g "No given description"))
