(ns satellite-clj.coverage
  "Matrix generation functions for satellite coverage analysis. Operations work
   primarily in radians.")

(defn haversine
  "Calculate the angular distance, in radians, between two points on the
   Earth's surface, also in radians.

     phi - latitude in radians
     lam - longitude in radians

   Example:
     (haversine 0 1 1 0)
     ;=> 1.2745557823062945"
  [phi-1 lam-1 phi-2 lam-2]
  (let [phi-1 (double phi-1)
        lam-1 (double lam-1)
        phi-2 (double phi-2)
        lam-2 (double lam-2)
        delta-phi (Math/pow (Math/sin (/ (- phi-2 phi-1) 2)) 2)
        delta-lam (Math/pow (Math/sin (/ (- lam-2 lam-1) 2)) 2)
        a (+ delta-phi (* (Math/cos phi-1) (Math/cos phi-2) delta-lam))]
    (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))))

(defn horizon
  "Calculate the distance to horizon, in radians, for a satellite given its
   altitude, in kilometers.

   Example:
     (horizon 35786)
     ;=> 1.4190895207111016"
  [alt]
  (let [alt (double alt)
        r 6371]
    (Math/acos (/ r (+ r alt)))))

(defn view-fn
  "Create a function to determine if a satellite is in view of a point on the
   Earth's surface. Returns 1 if in view, otherwise 0. Takes 3 arguments:

     lat - latitude of the satellite, in degrees
     lon - longitude of the satellite, in degrees
     alt - altitude of the satellite, in kilometers

   The returned function takes two arguments:

     p - latitude of the point on the Earth's surface, in radians
     l - longitude of the point on the Earth's surface, in radians

   Example:
     (let [visible? (view-fn 10 25 35786)]
       (println (visible? 0 0))
       (println (visible? 0.7853981633974483 3.0543261909900767)))
     ;=> 1
     ;=> 0"
  [lat lon alt]
  (let [lim (horizon alt)
        phi (Math/toRadians (double lat))
        lam (Math/toRadians (double lon))]
    (fn [p l] (if (<= (double (haversine phi lam p l)) (double lim)) 1 0))))

(defn blank-matrix
  "Return an Java array containing y rows and x columns.

   Example:
     (let [m (blank-matrix 5 10)]
       (vec (map vec m)))
     ;=>[[0 0 0 0 0 0 0 0 0 0]
         [0 0 0 0 0 0 0 0 0 0]
         [0 0 0 0 0 0 0 0 0 0]
         [0 0 0 0 0 0 0 0 0 0]
         [0 0 0 0 0 0 0 0 0 0]]"
  [y x]
  (make-array Integer/TYPE y x))

(defn coverage-matrix
  "Create a combined coverage matrix for a list of satellite coordinates, using
   a given resolution; top left of the matrix is [90, -180]. Takes
   two arguments:

     res - height of the output matrix (width= 2x res)
     coords - a nested list of satellite lat, lon, and alt in degrees and km

   Example:
     (let [cm (coverage-matrix 10 [[0 0 35786] [-45 90 1200]])]
       (vec (map vec cm)))
     ;=> [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0]
          [0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0]
          [0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0]
          [0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0]
          [0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0]
          [0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 1 1 0 0 0]
          [0 0 0 0 0 0 1 1 1 1 1 1 1 2 2 1 1 1 0 0]
          [0 0 0 0 0 0 1 1 1 1 1 1 1 2 2 1 1 1 0 0]
          [0 0 0 0 0 0 0 1 1 1 1 1 1 2 1 1 1 1 0 0]]"
  [res coords]
  (let [y (int res)
        x (int (* 2 res))
        m (blank-matrix y x)]
    (if (seq coords)
      (let [vfns (map #(apply view-fn %) coords)
            pm (/ Math/PI (- y))
            ps (/ Math/PI 2)
            lm (/ (* 2 Math/PI) x)
            ls Math/PI
            p-scale #(+ (* % pm) ps)
            l-scale #(- (* % lm) ls)]
        (doseq [yc (range 0 y) xc (range 0 x)]
          (let [p (p-scale yc)
                l (l-scale xc)
                v (reduce + (map #(% p l) vfns))]
            (aset-int m yc xc v)))
        (vec (map vec m)))
      (vec (map vec m)))))

(defn print-matrix
  "Print a coverage matrix to STDOUT; useful for debugging.

   Example:
     (let [cm (coverage-matrix 10 [[0 0 35786] [-45 90 1200]])]
       (print-matrix cm))
     ;=> 00000000000000000000
     ;=> 00000001111111000000
     ;=> 00000011111111100000
     ;=> 00000011111111100000
     ;=> 00000011111111100000
     ;=> 00000011111111100000
     ;=> 00000011111111211000
     ;=> 00000011111112211100
     ;=> 00000011111112211100
     ;=> 00000001111112111100"
  [m]
  (let [lines (map #(apply str %) m)]
    (doseq [l lines] (println l))))
