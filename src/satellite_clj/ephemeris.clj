(ns satellite-clj.ephemeris
  "Functions for satellite ephemeris propagation."
  (:require [satellite-clj.math :as m]
            [satellite-clj.orbit :as orbit]
            [satellite-clj.time :as time])
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
  [tle date]
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
    [lat lon alt]))

;; Perturbations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sma-rate
  [semi-major-axis time-derivative]
  (let [a semi-major-axis
        nd (* 2 time-derivative)
        n (orbit/revolutions semi-major-axis)]
    (- (* (/ (* 2 a) (* 3 n)) nd))))

(defn ecc-rate
  [semi-major-axis eccentricity time-derivative]
  (let [e eccentricity
        nd (* 2 time-derivative)
        n (orbit/revolutions semi-major-axis)]
    (- (* (/ (* 2 (- 1 e)) (* 3 n)) nd))))

(defn raan-j2
  [semi-major-axis eccentricity inclination]
  (let [a semi-major-axis
        e eccentricity
        i inclination]
    (* -2.064734896e14 (Math/pow a -3.5)
       (Math/pow (- 1 (* e e)) -2) (Math/cos (m/deg->rad i)))))

(defn perigee-j2
  [semi-major-axis eccentricity inclination]
  (let [a semi-major-axis
        e eccentricity
        i inclination]
    (* 1.032367448e14 (Math/pow a -3.5) (Math/pow (- 1 (* e e)) -2)
       (- 4 (* 5 (Math/pow (Math/sin (m/deg->rad i)) 2))))))

;; Propagation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn two-body
  [{:keys [t a e i o w v]} prop-date]
  (let [v-fut (orbit/true-anomaly-future a e v t prop-date)]
    {:t prop-date :a a :e e :i i :o o :w w :v v-fut}))

(defn two-body-j2
  [{:keys [t a e i o w v]} time-derivative prop-date]
  (let [t-delta (time/delta-days t prop-date)
        af (+ a (* (sma-rate a time-derivative) t-delta))
        ef (+ e (* (ecc-rate a e time-derivative) t-delta))
        of (mod (+ o (* (raan-j2 a e i) t-delta)) 360)
        wf (mod (+ w (* (perigee-j2 a e i) t-delta)) 360)
        n  (orbit/revolutions a)
        M  (orbit/mean-anomaly e v)
        Mt (+ (/ M 360) (* n t-delta) (* time-derivative t-delta t-delta))
        Mf (* 2 Math/PI (- Mt (int Mt)))
        E  (last (take 50 (iterate #(+ Mf (* ef (Math/sin %))) Mf)))
        vt (m/rad->deg (Math/acos (/ (- (Math/cos E) ef)
                                     (- 1 (* ef (Math/cos E))))))
        vf (mod (if (< (m/rad->deg E) 180) vt (- 360 vt)) 360)]
    {:t prop-date :a af :e ef :i i :o of :w wf :v vf}))
