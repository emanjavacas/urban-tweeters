(defproject urban-tweeters "0.1.0-SNAPSHOT"
  :description "Tweet-based urban multilingualism"
  :url "http://github.com/emanjavacas/urban-tweeters"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :resource-paths ["resources/"
                   "resources/files/"]
  :jvm-opts ["-Xmx4g" "-XX:MaxPermSize=128m" "-XX:+UseConcMarkSweepGC" "-XX:+CMSClassUnloadingEnabled"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.1"]
                 [org.clojure/data.csv "0.1.2"]
                 [clj-json "0.5.3"]
                 [clj-time "0.9.0"]
                 [org.clojars.brunchboy/colors "1.0.2-SNAPSHOT"]
                 [seesaw "1.4.5"]                 
                 [glgraphics "0.1"]
                 [json4proc "0.1"]
                 [log4j/log4j "1.2.17"]
                 [quil "2.2.5"]
                 [unfolding "0.9.6"]
                 [controlP5 "2.0.4"]]
  :main urban.menu
  :aot [urban.protocols urban.menu])
