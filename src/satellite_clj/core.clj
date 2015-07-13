(ns satellite-clj.core
  "Contains aliases and helper functions for common satellite-clj functions."
  (:require [satellite-clj.coordinates :as coord]
            [satellite-clj.time :as time]))

;;;; Coordinate Transforms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Geodetic
(defmulti ->geodetic first)

(defmethod ->geodetic :geodetic [coords]
  coords)

(defmethod ->geodetic :ecef [coords]
  (coord/ecef->geodetic coords))

(defmethod ->geodetic :eci [coords]
  (-> (coord/eci->ecef coords) coord/ecef->geodetic))

(defmethod ->geodetic :rv [coords]
  (let [coords (time/append-time coords)]
    (-> [:eci (:r (second coords)) (last coords)] ->geodetic)))

(defmethod ->geodetic :kepler [coords]
  (let [coords (time/append-time coords)]
    (-> (coord/kepler->rv coords) ->geodetic)))

;; Earth Centered Earth Fixed (ECEF)
(defmulti ->ecef first)
