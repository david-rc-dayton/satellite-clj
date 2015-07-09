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
  [[i j k] date]
  (let [g (time/gmst date)]
    [(+ (* i (Math/cos g)) (* j (Math/sin g)))
     (+ (* i (- (Math/sin g))) (* j (Math/cos g)))
     k]))

(defn rv->kepler
  [r v t]
  {:t t
   :a (orbit/semi-major-axis r v)
   :e (m/mag (orbit/ecc-vector r v))
   :i (orbit/inclination r v)
   :o (orbit/right-ascension r v)
   :w (orbit/argument-of-perigee r v)
   :v (orbit/true-anomaly r v)})
