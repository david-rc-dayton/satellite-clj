(ns satellite-clj.properties
  "Physical properties and units used in calculation."
  (:require [satellite-clj.time :as time]))

(def grav 6.67259e-20)

(def g-accel 9.80665)

(def wgs84
  (let [semi-major-axis 6378.137
        inverse-flattening 298.257223563
        semi-minor-axis (* semi-major-axis (- 1 (/ 1 inverse-flattening)))
        mean-radius (/ (+ (* 2 semi-major-axis) semi-minor-axis) 3)
        first-eccentricity-squared (- 1 (* (/ semi-minor-axis semi-major-axis)
                                           (/ semi-minor-axis semi-major-axis)))
        gravatational-parameter 398600.4418]
    {:a semi-major-axis
     :b semi-minor-axis
     :r mean-radius
     :e2 first-eccentricity-squared
     :mu gravatational-parameter}))

(def solar-properties
  "Orbital properties of the Earth relative to the Sun. Available keys for the
   returned `solar-properties` map are:

   > `:mean-distance` - Average distance of the Earth from the Sun, in meters  
   > `:eccentricity` - Eccentricity of the Earth's orbit around the Sun  
   > `:orbital-period` - Duration of the Earth's orbit around the Sun, in days  
   > `:declination` - Solar elevation above the Equator at solstice, in degrees"
  {:mean-distance 149598261
   :eccentricity 0.01671123
   :orbital-period 365.256363004
   :declination 23.439281})

(defn solar-latitude
  "Calculate the latitude of the Sun. Takes a `java.util.Date` object as its
   only argument.

   Returns the Sun's latitude at the given time, in degrees."
  [date]
  (let [d (+ (time/day-of-year date)
             (* (:orbital-period solar-properties) 3/4) 10)
        r (/ (* 2 Math/PI) (:orbital-period solar-properties))
        t (:declination solar-properties)]
    (* t (Math/sin (* r d)))))

(defn solar-longitude
  "Calculate the longitude of the Sun. Takes a `java.util.Date` object as its
   only argument.

   Returns the Sun's longitude at the given time, in degrees."
  [date]
  (let [d (time/day-of-year date)
        p (:orbital-period solar-properties)
        b (/ (* 2 Math/PI (- (int d) (- (* p 1/4) 10))) p)
        eot (- (* 9.87 (Math/sin (* 2 b))) (* 7.53 (Math/cos b))
               (* 1.5 (Math/sin b)))
        t (+ (- 360 (mod (* (+ (- d (int d)) (/ eot 1440)) 360) 360)) 180)]
    (cond
      (<= t -180) (+ t 360)
      (> t 180) (- t 360)
      :else t)))

(defn solar-altitude
  "Calculate the distance between the Earth and the Sun. Takes a
   `java.util.Date` object as its only argument.

   Returns the Sun's altitude at the given time, in meters."
  [date]
  (let [dn (time/day-of-year date)
        ro (:mean-distance solar-properties)
        ec (:eccentricity solar-properties)
        T (:orbital-period solar-properties)
        t (+ (* T 1/4) 10)
        pi (Math/PI)]
    (* ro (inc (* ec (Math/sin (/ (* 2 pi (- dn t)) T)))))))

(defn solar-position
  "Calculate the location of the Sun relative to the Earth. Takes a
   `java.util.Date` object as its only argument.

   Returns a map containing the keys `:lat :lon :alt` of the Sun at the given
   time, in degrees and meters."
  ([date]
    {:lat (solar-latitude date) 
     :lon (solar-longitude date)
     :alt (solar-altitude date)}))
