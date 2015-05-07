
* Namespace declaration and utils functions
#+BEGIN_SRC clojure :results silent
(ns twitviz.stats
  (:require [my-utils.io :refer :all]
            [my-utils.syntax :refer :all]
	    [clojure.string :as s]
	    [clojure.data.json :as json]
            [clojure.data.csv :as csv]
            [incanter
             [core :refer :all]
             [charts :refer :all]
             [stats :refer :all]
             [datasets :refer :all]]))
#+END_SRC

#+BEGIN_SRC clojure :results silent
(defn fetch-hoods [city]
  (let [cities {"berlin" "resources/hoods/berlin.geojson"
                "berlin-light" "resources/hoods/berlin.json"
                "amsterdam" "resources/hoods/amsterdam.geojson"
                "antwerp" "resources/hoods/antwerp.geojson"
                "brussels" "resources/hoods/bruxelles.geojson"}
        cityjson (json/read-json (slurp (get cities city)))]
    (map (fn [hood] {:meta (get-in hood [:properties :name]) 
                     :geometry (vec (for [[y x] (first (get-in hood [:geometry :coordinates]))]
                                      [x y]))})
         (:features cityjson))))

(defn load-grid [grid-fn]
  (let [grid (atom {})
        lines (lazy-lines grid-fn)
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
#+END_SRC


* Implementation of Theil multigroup entropy index

** Description of the measure

For a succint description see [[http://www.census.gov/hhes/www/housing/housing_patterns/multigroup_entropy.pdf][this]] from the census.gov website.
For a scientific reference and foundation of the metric see (Reardon & Firebaugh 2002 and M. White 1983)

The following formulae implement the Theil's multigroup entropy index as per the following equation.


\[
H = \sum\limits_{j=1}^J \frac{t_j}{TE}(E - E_j)
\]

where J is the number of organizational units (districts, cells, etc...), t_j is the total number of individuals
in organizational unit j, T is total number of organizational units, E is the total diversity and E_j is the
diversity observed in organizational unit j.

Since we are using Theil's entropy index, the measure for diversity (E) is computed with the following
information theoretical formula:

\[
E = \sum\limits_{m=1}^M \pi_m  ln (\frac{1}{\pi_m})
\]

where M is the total number of groups in the population (in our case it amounts to languages) and \pi is the proportion
of group m in the population (note that normally the natural log is taken).

Note to myself, in NLP, entropy is normally shown as 
\[
E = -\sum\limits_{m=1}^M \pi_m  ln (\pi_m)
\]

In Reardon & Firebaugh 2002 (pg. 44ff), this entropy-based multigroup segregation index is derived as diversity ratio.
First, a measure of the "diversity" is defined (in our case entropy) and, then, segregation is defined as
the share of this diversity accounted for by the differences in group proportions across units.

The diversity measure is assumed to be defined as a function of the population shares of each of the groups
and it should acquire its minimum value of zero where all individuals are members of the same group
and its maximum value when each group is equally represented in the population. Which certainly holds for entropy.

Given a diversity measure d, a population of M groups, with a total of T individuals distributed
across T organizational units, it can be shown that if d is continous, differentiable and cooncave-down, then
\[
0 \leq \sum\limits_{j=1}^J \frac{t_j}{T} d_j \geq d
\]

which means that the weighted average of of the within-unit diversities will
always be greater than or equal to zero, with equality holding if each unit has no diversity,
and less than or equal total diversity, with equality holding when all within-unit proportions are
equal to the global group proportions.

Taking advantage of this fact, one can state a global segregation measure
\[
S(d) = 1 - \frac{\bar{d_j}}{d} = \sum\limits_{j=1}^J \frac{t_j}{Td} (d - dj)
\]

which equals the first equation when d is E.

** Properties
Theil's multigroup diversity index is bounded by zero and one.

** Implementation

#+BEGIN_SRC clojure :results silent
(defn entropy-score
  "computes total entropy for a distribution of counts"
  [ps]
  (reduce + (map (fn [p] (* p (Math/log (/ 1 p)))) ps)))
  
(defn gs->ps
  "normalizes by total count"
  ([gs] (gs->ps gs (reduce + gs)))
  ([gs total]
   (map #(/ % total) gs)))

(defn entropy-index
  "computes theil's entropy index for a collection
  of distribution of counts"
  [gms]
  (let [T (reduce + (mapcat vals gms))
        E (entropy-score (gs->ps (mapcat vals gms) T))
        ET (* E T)]
    (reduce + (map (fn [m]
                     (let [t (reduce + (vals m))
                           e (entropy-score (gs->ps (vals m) t))]
                       (/ (* t (- E e))
                          ET)))
                   gms))))
#+END_SRC

* Computations
** Based on tweets

- Fetch the data
 #+BEGIN_SRC clojure :results silent
(def berlin (load-grid "resources/berlin.grid"))
(def berlin-hoods (fetch-hoods "berlin"))
(def berlin-hoods-light (fetch-hoods "berlin-light"))
(def berlin-by-hood (frm-load "berlin_by_polys_light.tweets"))
 #+END_SRC

- The distribution of cells by district is then
#+BEGIN_SRC clojure :results value
(map #(count (vals (second %))) berlin-by-hood)
#+END_SRC

#+RESULTS:
: (440 453 439 480 553 488 348 242 280 451 473 355)270043

- The total number of points in which the computation is based.
#+BEGIN_SRC clojure
(reduce + (mapcat vals (mapcat vals (vals berlin-by-hood))))
#+END_SRC

#+RESULTS:
: 270043

- Compute entropies by neighbourhood
#+BEGIN_SRC clojure :results silent
(def result-tweets
  (zipmap (keys berlin-by-hood)
          (map entropy-index (map vals (vals berlin-by-hood)))))
#+END_SRC

** Based on register data

- Read in data
#+BEGIN_SRC clojure :results silent
(def berlin-register (frm-load "berlin_by_districts.register"))
#+END_SRC

- Compute entropy (normalizing hood names)
#+BEGIN_SRC clojure :results silent
(def result-register (zipmap (map #(second (clojure.string/split % #" ")) (keys berlin-register))
                     (map entropy-index (map vals (vals berlin-register)))))
#+END_SRC


#+BEGIN_SRC clojure :results silent
(def result-merged (merge-with vector result-tweets result-register))
(clojure.pprint/pprint result-merged)
#+END_SRC

#+RESULTS:
#+begin_example
{"Charlottenburg-Wilmersdorf" [0.834134741933901  0.7398183167160721],
 "Spandau"                    [0.9084754000487446 0.7780782138734974],
 "Pankow"                     [0.8762883761933832 0.8380497077104752],
 "Mitte"                      [0.8269929167774379 0.6936442588657165],
 "Steglitz-Zehlendorf"        [0.9019247857002738 0.8028626412058488],
 "Reinickendorf"              [0.8803239786996437 0.7896145263556944],
 "Neukölln"                   [0.8500908792523975 0.7343981458784431],
 "Friedrichshain-Kreuzberg"   [0.8226599155950466 0.7116018131026864],
 "Lichtenberg"                [0.8914504298018489 0.8382789317832962],
 "Tempelhof-Schöneberg"       [0.8696219289667678 0.7569444211626952],
 "Treptow-Köpenick"           [0.913796737368426  0.8990007985980895],
 "Marzahn-Hellersdorf"        [0.9395921710538039 0.8818968964340289]}
#+end_example

#+BEGIN_SRC clojure :results silent
(view (bar-chart (mapcat identity (repeat 2 (keys result-merged)))
                 (concat (map first (vals result-merged)) (map second (vals result-merged)))
                 :group-by (concat (repeat 12 "Tweets") (repeat 12 "Register"))
                 :legend true))
#+END_SRC

- Pearson Correlation

#+BEGIN_SRC clojure
(correlation (map first (vals result-merged)) (map second (vals result-merged)))
#+END_SRC

#+RESULTS:
: 0.870187217188301

- Spearman's Rho
#+BEGIN_SRC clojure
(spearmans-rho (map first (vals result-merged)) (map second (vals result-merged)))
#+END_SRC

#+RESULTS:
: 0.8741258741258742

* Other
** Computing the dataframe

- Read in the tabulated data from the register and compute counts by subsubsubdistrict (a total of 442)
  in each district (a total of 12)

# NOT RUN!!
#+BEGIN_SRC clojure :results silent
(with-open [rdr (clojure.java.io/reader "/Users/quique/data/berlin_zensus/dataset3.csv")]
  (let [header (first (csv/read-csv rdr))
        dataset (doall (next (csv/read-csv rdr)))
        coerce-fn (fn [[a b c d e f]]
                    [a b (Integer/parseInt c) (Integer/parseInt d)
                     (Integer/parseInt e) (Integer/parseInt f)])]
    (def dataset (vec (cons header (mapv coerce-fn dataset))))))

;; {"District" {"Subdistrict1" {"zh" 123} {"es" 12}}}
(def districts (into #{} (map second (filter #(= 2 (last %)) dataset))))
(def subdistricts (into #{} (map second (filter #(= 8 (last %)) dataset))))
(def langs (into #{} (map first dataset)))

(let [my-dataset (atom {})]
  (doseq [l langs
          district districts
          :let [regex (re-pattern (str "^" (first (clojure.string/split district #" ")) ".*"))]]
    (doseq [row (next dataset)
            :let [subdistrict (second row)
                  v (reduce + (subvec row 2 5))]
            :when (and
                   (not (zero? v))
                   (= 8 (last row))
                   (re-matches regex (second row))
                   (= l (first row)))]
      (swap! my-dataset syn/deep-merge {district {subdistrict {l v}}})))
  (def my-data @my-dataset))
(io/frm-save "berlin_by_districts.register" my-data)
#+END_SRC

- Sanity check, compute total number of inhabitants
#+BEGIN_SRC clojure
(map #(count (vals (second %))) my-data)
#+END_SRC
