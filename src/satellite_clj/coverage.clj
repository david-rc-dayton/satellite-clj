(ns satellite-clj.coverage
  "Matrix generation functions for satellite coverage analysis. Operations work
   primarily in radians.")

;;;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pi
  "Value of π."
  (Math/PI))

(def two-pi
  "Value of 2π."
  (* 2 pi))

(def half-pi
  "Value of π/2."
  (/ pi 2))

(def ascii-legend
  "Map containing the legend of characters for use in the
   print-coverage-matrix function."
  (let [k [0   1   2   3   4   5   :default]
        v ["." ":" "=" "%" "+" "V" "@"]]
    (atom (zipmap k (map #(str % " ") v)))))

;;;; Terminal Display ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn merge-ascii-legend!
  "Merge the current ascii-legend with a new character map, for use in the
   print-coverage-matrix function. For example:

   Use the & and $ symbols for cells in view of 8 and 9 satellites:  
     (merge-ascii-legend! {8 \"&\", 9 \"$\"})

   Change the default symbol for coverage not defined in ascii-legend to ?:  
     (merge-ascii-legend! {:default \"?\"})"
  [merge-map]
  (swap! ascii-legend merge
         (zipmap (keys merge-map) (map #(str % " ") (vals merge-map)))))

(defn print-coverage-matrix
  "Format and print coverage matrix m to the output stream assigned to *out*
   (typically stdout). Characters indicating the amount of coverage at a given
   point are defined in the ascii-legend map."
  [m]
  (let [replace-fn #(or (get @ascii-legend %) (:default @ascii-legend))
        str-fn #(clojure.string/join (map replace-fn %1))]
    (dorun (map #(println (str-fn %)) m))))

;;;; Coverage Element Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cov-cosine
  "Calculate the angular distance between two points using the Law of Cosines.
   Takes four arguments, in radians:

     phi-1 - latitude of the starting point  
     lam-1 - longitude of the starting point  
     phi-2 - latitude of the ending point  
     lam-2 - longitude of the ending point

   Returns the angular distance between two points in radians."
  [phi-1 lam-1 phi-2 lam-2]
  (let [delta-phi-two (Math/pow (Math/sin (double (/ (- phi-2 phi-1) 2))) 2)
        delta-lam-two (Math/pow (Math/sin (double (/ (- lam-2 lam-1) 2))) 2)
        a (+ delta-phi-two (* (Math/cos (double phi-1))
                              (Math/cos (double phi-2)) delta-lam-two))]
    (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))))

(defn cov-haversine
  "Calculate the angular distance between two points using the Haversine
   Formula. Takes four arguments, in radians:

     phi-1 - latitude of the starting point  
     lam-1 - longitude of the starting point  
     phi-2 - latitude of the ending point  
     lam-2 - longitude of the ending point

   Returns the angular distance between two points in radians."
  [phi-1 lam-1 phi-2 lam-2]
  (let [delta-phi (Math/sin (double (* 0.5 (- phi-2 phi-1))))
        delta-lam (Math/sin (double (* 0.5 (- lam-2 lam-1))))
        a (+ (* delta-phi delta-phi)
             (* (Math/cos (double phi-1)) (Math/cos (double phi-2))
                delta-lam delta-lam))]
    (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (double (- 1 a)))))))

(def cov-methods
  "Map associating keywords with a method of coverage calculation. Available
   methods are:

     :cosine - Law of Cosines (fast)  
     :haversine - Haversine Formula (slow)"
  {:cosine    cov-cosine
   :haversine cov-haversine})

(defn view-fn
  "Generates a function with cached values for coverage calculation, for faster
   execution over a matrix. Takes two arguments:

     method - keyword for the method of calculation (see cov-methods)  
     {:lat :lon :alt} - location map of the satellite, in degrees and meters

   Returns a function that returns the angular distance between the satellite
   (previously entered) and a point on the Earth's surface. Takes two arguments
   in radians:

     gnd-lat - latitude of the point on the Earth's surface  
     gnd-lon - longitude of the point on the Earth's surface"
  [method [lat lon alt]]
  (let [sat-lat (double (* lat (/ pi 180)))
        sat-lon (double (* lon (/ pi 180)))
        view-limit (Math/acos (double (/ 6371000 (+ 6371000 alt))))
        view-method (get cov-methods method)]
    (fn [gnd-lat gnd-lon]
      (if (<= (view-method sat-lat sat-lon gnd-lat gnd-lon)
              view-limit) 1 0))))

;;;; Coverage Matrix Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rangef
  "Modified range function to create arrays of floating-point values with the
   correct number of elements. Takes three arguments:

     start - starting point of the range  
     end - ending point of the range  
     step - amount to increment or decrement each number in the sequence

   Returns a list of floating-point numbers."
  [start end step]
  (map #(+ start (* % step))
       (range 0 (/ (Math/abs (double (- end start)))
                   (Math/abs (double step))) 1)))

(defn blank-matrix
  "Generate a blank coverage matrix. Takes the argument:

     dimensions - a vector of the width & height for the desired matrix

   Returns a matrix containing coordinates of the form [latitude [longitudes]],
   in radians."
  [[width height]]
  (let [step-width (/ two-pi width)
        step-height (/ pi height)
        lon (rangef (- pi) pi step-width)]
    (for [lat (rangef half-pi (- half-pi) (- step-height))]
      (list lat lon))))

(defn combine-matrix
  "Add matrices a and b. A matrix is entered as a two-dimensional nested
   list; the number of rows and columns in a and b must be equal.

   Returns the sum of the two matrices, as a two-dimensional nested list."
  [a b]
  (map #(map + %1 %2) a b))

(defn coverage-single
  "Generates a matrix of a satellite's coverage over the Earth's surface. Takes
   three arguments:

     method - keyword for the method of calculation (see cov-methods)  
     sat-location - map of the satellite's location (see view-fn)  
     dimensions - a vector containing the width & height of the output matrix

   Returns a matrix representing global satellite coverage from [-90 90] degrees
   latitude and [-180 180] degrees longitude, centered at the Equator and the
   Prime Meridian respectively. Elements in view are denoted by a 1,
   otherwise 0."
  [method sat-location dimensions]
  (let [matrix (blank-matrix dimensions)
        in-view? (view-fn method sat-location)
        cov-fn #(map in-view? (repeat (first %)) (second %))]
    (map cov-fn matrix)))

(defn coverage-combined
  "Generates a matrix of multiple satellites' coverage over the Earth's surface.
   Takes three arguments:

     method - keyword for the method of calculation (see cov-methods)  
     sat-locations - a list of satellite location maps (see view-fn)  
     dimensions - a vector containing the width & height of the output matrix

   Returns a matrix representing combined global satellite coverage from
   [-90 90] degrees latitude and [-180 180] degrees longitude, centered at the
   Equator and the Prime Meridian respectively. Elements contain the number of
   satellites in view of the associated region."
  [method sat-locations dimensions]
  (let [cov-fn #(coverage-single method % dimensions)]
    (reduce combine-matrix (map cov-fn sat-locations))))

(defn coverage-matrix
  "Generates a matrix of a single, or multiple satellites' coverage over the
   Earth's surface. Takes three arguments:

     method - keyword from cov-methods  
     sat-locations - satellite location map (individual, or a list of maps)  
     dimensions - a vector containing the width & height of the output matrix

   Returns a matrix representing combined global satellite coverage from
   [-90 90] degrees latitude and [-180 180] degrees longitude, centered at the
   Equator and the Prime Meridian respectively. Elements contain the number of
   satellites in view of the associated region."
  [method sat-location dimensions]
  (let [cov-fn (cond
                 (map? sat-location)  coverage-single
                 (coll? sat-location) coverage-combined)]
    (cov-fn method sat-location dimensions)))
