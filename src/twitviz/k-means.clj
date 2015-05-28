(ns twitviz.k-means)

(defn great-circle-distance
  "gettingclojure.wikidot.com/cookbook:numbers"
  ([[lat1 long1] [lat2 long2]] (great-circle-distance lat1 long1 lat2 long2 6371.009))
  ([[lat1 long1] [lat2 long2] radius]
     (let [[lat1-r long1-r lat2-r long2-r]
           (map #(Math/toRadians %) [lat1 long1 lat2 long2])]
       (* radius
          (Math/acos (+ (* (Math/sin lat1-r) (Math/sin lat2-r))
                        (* (Math/cos lat1-r) (Math/cos lat2-r) (Math/cos (- long1-r long2-r)))))))))

(defn- closest-centroid
  "closest centroid to a given point"
  [p centroids dist-fn]
  (first (sort-by #(dist-fn % p) centroids)))

(defn- update-centroid
  "compute the n-dim centroid given a coll of n-dim points"
  [& ps]
  (let [n (count ps)]
    (map #(/ % n) (apply map + ps))))

(defn cluster-points
  "map points to their closest centroid"
  [ps centroids dist-fn]
  (group-by #(closest-centroid % centroids dist-fn) ps))

(defn update-centroids [ps centroids dist-fn]
  (map update-centroid (vals (cluster-points ps centroids dist-fn))))

(defn converge
  "returns the items in until the 
  first adjacent repetition"
  [coll & d]
  (lazy-seq
   (when-let [sq (seq coll)]
     (if (and d (= (first sq) (first d))) '()
         (cons (first sq)
               (converge (rest sq) (first sq)))))))

(defmulti abs-dist (fn [a b] [(type a) (type b)]))
(defmethod abs-dist [clojure.lang.PersistentVector
                     clojure.lang.PersistentVector]
  ([a b] (reduce + (map #(Math/abs %) (map - a b)))))
(defmethod abs-dist [Long Long] ([a b] (Math/abs (- a b))))

(defn iterate-step [centroids] (update-centroids data centroids abs-dist))

;; (def data '([2 3] [5 6] [10 11] [100 101] [1 102]))
;; (take 2 (iterate iterate-step [[0 10] [3 5]]))
