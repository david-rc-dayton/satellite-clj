(ns satellite-clj.ephemeris
  "Clojure wrapper to the *predict4java*
   [SGP4](http://en.wikipedia.org/wiki/Simplified_perturbations_models)
   satellite ephemeris propagation library."
  (:require [satellite-clj.math :as m])
  (:import [uk.me.g4dpz.satellite SatelliteFactory TLE]))

;;;; NORAD TLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn str->tle
  "Convert a Two Line Element (TLE) set, into a usable predict4java TLE
   object. Takes a single vector containing text strings of the form
   [name first-line second-line] where:

     name - The name of the spacecraft  
     first-line - Line 1 of the orbital elements  
     second-line - Line 2 of the orbital elements

   Returns a predict4java TLE object for use with the SGP4 propagator."
  [tle]
  (TLE. ^"[Ljava.lang.String;" (into-array String tle)))

(defn valid-tle?
  "Determine if a Two Line Element Set (TLE) is valid based on the checksum
   value for each line. Takes a single vector containing text strings of the
   form [name first-line second-line] where:

     name - The name of the spacecraft  
     first-line - Line 1 of the orbital elements  
     second-line - Line 2 of the orbital elements

   Returns true if the TLE vector appears properly formatted and passes a
   checksum."
  [[name first-line second-line :as tle]]
  (let [char->int #(Character/getNumericValue ^char %)
        digits (set (map char (range 48 58)))
        replace-dash #(clojure.string/replace % "-" "1")
        valid? #(= (mod (reduce + (butlast %)) 10) (last %))
        tle-clean (->> (map replace-dash (rest tle))
                    (map #(filter digits (apply vector %)))
                    (map #(map char->int (apply vector %))))]
    (and (not (clojure.string/blank? (.trim (str name))))
         (not (clojure.string/blank? (.trim (str first-line))))
         (not (clojure.string/blank? (.trim (str second-line))))
         (every? true? (map valid? tle-clean)))))

;;;; SGP4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sgp4
  "Propagate satellite ephemeris using a Two Line Element (TLE) set. Takes two
   arguments:

     tle - Vector containing tle data (see valid-tle?)  
     date - A java.util.Date object used as the propagation time

   Returns the satellite's location as a vector containing the [lat lon alt]
   in degrees and meters."
  ([tle date]
    (let [tle (str->tle tle)
          factory (doto (SatelliteFactory/createSatellite tle)
                    (.calculateSatelliteVectors date))
          position (.calculateSatelliteGroundTrack factory)
          lat (Math/toDegrees (.getLatitude position))
          temp-lon (Math/toDegrees (.getLongitude position))
          lon (cond
                (> temp-lon 180) (- temp-lon 360)
                (neg? temp-lon) (+ temp-lon 360)
                :else temp-lon)
          alt (.getAltitude position)]
      [lat lon alt])))

;; Perturbations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn raan-j2
  [{:keys [a e i]}]
  (* -2.064734896e14 (Math/pow a -3.5)
     (Math/pow (- 1 (* e e)) -2) (Math/cos (m/deg->rad i))))

(defn perigee-j2
  [{:keys [a e i]}]
  (* 1.032367448e14 (Math/pow a -3.5) (Math/pow (- 1 (* e e)) -2)
     (- 4 (* 5 (Math/pow (Math/sin (m/deg->rad i)) 2)))))
