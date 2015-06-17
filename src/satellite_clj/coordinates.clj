(ns satellite-clj.coordinates
  "Functions for working with coordinates and their transforms."
  (:require [satellite-clj.math :as m]
            [satellite-clj.orbit :as orbit]
            [satellite-clj.properties :refer [wgs84]]
            [satellite-clj.time :as time]))

;;;; Coordinate Transforms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn wrap-geo
  [[lat lon alt]]
  (let [wrap-lon (cond
                   (> lon 180) (- lon 360)
                   (neg? lon)  (+ lon 360)
                   :else lon)]
    [lat wrap-lon alt]))

(defn geo-radius
  [latitude]
  (let [phi (m/deg->rad latitude)
        a (:a wgs84)
        b (:b wgs84)]
    (Math/sqrt (/ (+ (Math/pow (* a a (Math/cos phi)) 2)
                     (Math/pow (* b b (Math/sin phi)) 2))
                  (+ (Math/pow (* a (Math/cos phi)) 2)
                     (Math/pow (* b (Math/sin phi)) 2))))))

(defn geo->ecef
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
