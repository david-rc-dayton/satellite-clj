(ns satellite-clj.coordinates
  "Functions for working with coordinates and their transforms."
  (:require [satellite-clj.math :as m]
            [satellite-clj.orbit :as orbit]
            [satellite-clj.properties :refer [wgs84]]
            [satellite-clj.time :as time]))

;;;; Coordinate Transforms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn wrap-longitude
  "Convert a longitude to/from range [0 to 360] to [-180 to 180].

   Example:
     (wrap-longitude 270) ;=> -90
     (wrap-longitude -15) ;=> 345"
  [longitude]
  (cond
    (> longitude 180) (- longitude 360)
    (neg? longitude)  (+ longitude 360)
    :else longitude))

(defn geo-radius
  "Calculate the distance from the surface of the Earth, to the Earth's center
   for a given latitude. Based on the WGS84 reference ellipsoid; result is
   in kilometers.

   Example:
     (geo-radius 0) ;=> 6378.137
     (geo-radius -45) ;=> 6367.489543863465
     (geo-radius 90) ;=> 6356.752314245179"
  [latitude]
  (let [phi (m/deg->rad latitude)
        a (:a wgs84)
        b (:b wgs84)]
    (Math/sqrt (/ (+ (Math/pow (* a a (Math/cos phi)) 2)
                     (Math/pow (* b b (Math/sin phi)) 2))
                  (+ (Math/pow (* a (Math/cos phi)) 2)
                     (Math/pow (* b (Math/sin phi)) 2))))))

(defn geo->ecef
  "Convert geodetic coordinates to Earth Centered Earth Fixed (ECEF) coordinates
   in kilometers. Takes a vector containing the latitude, longitude, and
   altitude of a point in degrees and kilometers. Returns the X, Y, and Z
   components as a position vector.

   Example:
     (geo->ecef [10 20 2])
       ;=> [5904.880375850656 2149.2006937122637 1100.5958440906952]
     (geo->ecef [-45 85 402])
       ;=> [418.50861199604446 4783.575324270843 -4771.605334902912]"
  [[lat lon alt]]
  (let [re (:a wgs84)
        e-squared (:e2 wgs84)
        phi (m/deg->rad lat)
        lam (m/deg->rad lon)
        sin-phi (Math/sin phi)
        cos-phi (Math/cos phi)
        sin-lam (Math/sin lam)
        cos-lam (Math/cos lam)
        n (/ re (Math/sqrt (- 1 (* e-squared (Math/pow sin-phi 2)))))]
    [(* (+ n alt) cos-phi cos-lam)
     (* (+ n alt) cos-phi sin-lam)
     (* (+ (* n (- 1 e-squared)) alt) sin-phi)]))

(defn ecef->geo
  "Convert Earth Centered Earth Fixed (ECEF) coordinates to geodetic
   coordinates. Takes a vector containing the X, Y, and Z components of a
   point's position relative to the Earth's center, and returns the latitude,
   longitude, and altitude of the position vector in kilometers.

   Example:
     (ecef->geo [6370 -2345 7546])
       ;=> [48.14737988893906 -20.210269215157226 3783.4768939372952]
     (ecef->geo [1389.426 -4631.199 4145.635])
       ;=> [40.799999018658866 -73.3000032521367 -4.937954572596936E-4]"
  [[x y z]]
  (let [max-iter 10
        a (:a wgs84)
        e2 (:e2 wgs84)
        lam (Math/atan2 y x)
        p (m/mag [x y])
        phi-c (Math/atan2 p z)
        rn-fn #(/ a (Math/sqrt (- 1 (* e2 (Math/sin %) (Math/sin %)))))]
    (loop [phi-n phi-c dex 0]
      (if (> dex max-iter)
        [(m/rad->deg phi-n) (m/rad->deg lam) 
         (- (/ p (Math/cos phi-n)) (rn-fn phi-n))]
        (let [Rn (rn-fn phi-n) 
              h (- (/ p (Math/cos phi-n)) Rn)
              pn (Math/atan
                   (* (/ z p)
                      (Math/pow (- 1 (* e2 (/ Rn (+ Rn h)))) -1)))]
          (recur pn (inc dex)))))))

(defn ecef->eci
  "Convert Earth Centered Earth Fixed (ECEF) coordinates to Earth Centered
   Intertial (ECI) coordinates, in kilometers for a given date.

   Example:
     (def test-time #inst \"2006-09-26T00:00:00.000-00:00\")

     (ecef->eci [6378.137 0 0] test-time)
       ;=> [6354.52258010092 548.33782448099 0]"
  [[x y z] date]
  (let [g (time/gmst date)]
    [(- (* x (Math/cos g)) (* y (Math/sin g)))
     (+ (* x (Math/sin g)) (* y (Math/cos g)))
     z]))

(defn eci->ecef
  "Convert Earth Centered Inertial (ECI) coordinates to Earth Centered Earth
   Fixed (ECEF) coordinates, in kilometers for a given date.

   Example:
     (def test-time #inst \"2012-12-21T00:00:00.000-00:00\")

     (eci->ecef [0 6378.137 0] test-time)
       ;=> [6378.082623104259 -26.337114961469073 0]"
  [[i j k] date]
  (let [g (time/gmst date)]
    [(+ (* i (Math/cos g)) (* j (Math/sin g)))
     (+ (* i (- (Math/sin g))) (* j (Math/cos g)))
     k]))

(defn rv->kepler
  "Convert position and velocity vectors in Earth Centered Inertial (ECI)
   coordinates, in kilometers and kilometers per second, to Classical Keplerian
   Elements, for a given time. Returns a hash-map containing the following:

     :t - time at epoch (java.util.Date)
     :a - semi-major-axis (km)
     :e - eccentricity
     :i - inclination (degrees)
     :o - right ascension of the ascending node (degrees)
     :w - argument of perigee (degrees)
     :v - true anomaly (degrees)

   Example:
     (def eci-position [8228 389 6888])
     (def eci-velocity [-0.7 6.6 -0.6])
     (def test-time #inst \"1999-09-09T15:30:00.000-00:00\")

     (rv->kepler eci-position eci-velocity test-time)
     ;=> {:t #inst \"1999-09-09T15:30:00.000-00:00\",
     ;    :a 13360.642755150553,
     ;    :e 0.22049791761487267,
     ;    :i 39.93754927254844,
     ;    :o 269.85555147445865,
     ;    :w 125.72438209646339,
     ;    :v 326.46253404643056}"
  [r v t]
  {:t t
   :a (orbit/semi-major-axis r v)
   :e (m/mag (orbit/ecc-vector r v))
   :i (orbit/inclination r v)
   :o (orbit/right-ascension r v)
   :w (orbit/argument-of-perigee r v)
   :v (orbit/true-anomaly r v)})

(defn kepler->rv
  "Convert Classical Keplerian Elements to position and velocity vectors. Takes
   a hash-map containing the keys:

     :t - time at epoch (java.util.Date)
     :a - semi-major-axis (km)
     :e - eccentricity
     :i - inclination (degrees)
     :o - right ascension of the ascending node (degrees)
     :w - argument of perigee (degrees)
     :v - true anomaly (degrees)

   Returns a list containing the position vector, in kilometers, the
   velocity vector, in kilometers per second, and the time at epoch.

   Example:
     (def kep {:t #inst \"2015-07-13T19:09:21.062-00:00\",
               :a 8788.081767279667,
               :e 0.17121118195416893,
               :i 153.2492285182475,
               :o 255.27928533439618,
               :w 20.068139973005337,
               :v 28.44580498419213})
     (kepler->rv kep)
       ;=> [(-6045.0 -3490.0000000000023 2499.999999999997)
       ;    (-3.4570000000000025 6.6179999999999986 2.5329999999999977)
       ;    #inst \"2015-07-13T19:09:21.062-00:00\"]"
  [{:keys [t a e i o w v]}]
  (let [mu (:mu wgs84)
        E (Math/atan (/ (* (Math/sqrt (- 1 (* e e))) (Math/sin (m/deg->rad v)))
                        (+ e (Math/cos (m/deg->rad v)))))
        x (- (* a (Math/cos E)) (* a e))
        y (* a (Math/sin E) (Math/sqrt (- 1 (* e e))))
        r (m/mag [x y])
        n (orbit/mean-motion a)
        r-vec (->> (m/rot :z w [x y 0]) (m/rot :x i) (m/rot :z o))
        vx (* (- (/ (* a a n) r)) (Math/sin E))
        vy (* (/ (* a a n) r) (Math/sqrt (- 1 (* e e))) (Math/cos E))
        v-vec (->> (m/rot :z w [vx vy 0]) (m/rot :x i) (m/rot :z o))]
    [r-vec v-vec t]))
