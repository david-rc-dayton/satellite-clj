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
  (let [coords (time/append-time coords)]
    (-> (coord/eci->ecef coords) coord/ecef->geodetic)))

(defmethod ->geodetic :rv [coords]
  (let [coords (time/append-time coords)]
    (-> [:eci (:r (second coords)) (last coords)] ->geodetic)))

(defmethod ->geodetic :kepler [coords]
  (let [coords (time/append-time coords)]
    (-> (coord/kepler->rv coords) ->geodetic)))

;; Earth Centered Earth Fixed (ECEF)
(defmulti ->ecef first)

(defmethod ->ecef :ecef [coords]
  coords)

(defmethod ->ecef :geodetic [coords]
  (coord/geodetic->ecef coords))

(defmethod ->ecef :eci [coords]
  (let [coords (time/append-time coords)]
    (coord/eci->ecef coords)))

(defmethod ->ecef :rv [coords]
  (let [coords (time/append-time coords)]
    (-> [:eci (:r (second coords)) (last coords)] ->ecef)))

(defmethod ->ecef :kepler [coords]
  (let [coords (time/append-time coords)]
    (-> (coord/kepler->rv coords) ->ecef)))

;; Earth Centered Inertial (ECI)
(defmulti ->eci first)

(defmethod ->eci :eci [coords]
  (let [coords (time/append-time coords)]
    coords))

(defmethod ->eci :ecef [coords]
  (let [coords (time/append-time coords)]
    (coord/ecef->eci coords)))

(defmethod ->eci :geodetic [coords]
  (let [coords (time/append-time coords)]
    (-> (coord/geodetic->ecef coords) ->eci)))

(defmethod ->eci :rv [coords]
  (let [coords (time/append-time coords)]
    (-> [:eci (:r (second coords)) (last coords)] ->eci)))

(defmethod ->eci :kepler [coords]
  (let [coords (time/append-time coords)]
    (-> (coord/kepler->rv coords) ->eci)))

;; Position and Velocity (RV)
(defmulti ->rv first)

(defmethod ->rv :rv [coords]
  (let [coords (time/append-time coords)]
    coords))

(defmethod ->rv :kepler [coords]
  (let [coords (time/append-time coords)]
    (coord/kepler->rv coords)))

;; Classical Keplerian Elements
(defmulti ->kepler first)

(defmethod ->kepler :kepler [coords]
  (let [coords (time/append-time coords)]
    coords))

(defmethod ->kepler :rv [coords]
  (let [coords (time/append-time coords)]
    (coord/rv->kepler coords)))
