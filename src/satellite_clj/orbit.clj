(ns satellite-clj.orbit
  "Functions for satellite orbit operations."
  (:require [satellite-clj.math :as m]
            [satellite-clj.properties :refer [wgs84]]))

(defn period
  [semi-major-axis]
  (let [a semi-major-axis
        mu (:mu wgs84)]
    (/ (* 2 Math/PI (Math/sqrt (/ (* a a a) mu))) 86400)))

(defn revolutions
  [semi-major-axis]
  (/ 1  (period semi-major-axis)))

(defn mean-motion
  [semi-major-axis]
  (let [a semi-major-axis
        mu (:mu wgs84)]
    (Math/sqrt (/ mu (* a a a)))))

(defn eccentric-anomaly
  [eccentricity true-anomaly]
  (let [e eccentricity
        v (m/deg->rad true-anomaly)
        E (m/rad->deg (Math/acos (/ (+ e (Math/cos v))
                                        (inc (* e (Math/cos v))))))]
    (if (< true-anomaly 180) E (- 360 E))))

(defn mean-anomaly
  [eccentricity true-anomaly]
  (let [e eccentricity
        v true-anomaly
        E (m/deg->rad (eccentric-anomaly e v))]
    (m/rad->deg (- E (* e (Math/sin E))))))

(defn mean-anomaly-future
  [semi-major-axis eccentricity true-anomaly epoch prop-date]
  (let [a semi-major-axis
        e eccentricity
        Mi (m/deg->rad (mean-anomaly e true-anomaly))
        n (mean-motion a)
        TOF (/ (- (.getTime ^java.util.Date prop-date)
                  (.getTime ^java.util.Date epoch)) 1000)
        Mf (+ Mi (* n TOF))
        k (int (/ Mf (* 2 Math/PI)))]
    (m/rad->deg (- Mf (* 2 Math/PI k)))))

(defn eccentric-anomaly-future
  [semi-major-axis eccentricity true-anomaly epoch prop-date]
  (let [epsilon 1e-10
        a semi-major-axis
        e eccentricity
        v true-anomaly
        M (m/deg->rad (mean-anomaly-future a e v epoch prop-date))]
    (loop [Ef M]
      (let [En (+ M (* e (Math/sin Ef)))]
        (if (< (Math/abs (- Ef En)) epsilon)
          (m/rad->deg En)
          (recur En))))))

(defn true-anomaly-future
  [semi-major-axis eccentricity true-anomaly epoch prop-date]
  (let [a semi-major-axis
        e eccentricity
        v true-anomaly
        E (m/deg->rad (eccentric-anomaly-future a e v epoch prop-date))]
    (m/rad->deg (Math/acos (/ (- (Math/cos E) e)
                                  (- 1 (* e (Math/cos E))))))))

(defn mech-energy
  [r v]
  (let [V-sq (Math/pow (m/mag v) 2)
        R (m/mag r)
        mu (:mu wgs84)]
    (- (/ V-sq 2) (/ mu R))))

(defn semi-major-axis
  [r v]
  (let [mu (:mu wgs84)
        en (mech-energy r v)]
    (- (/ mu (* 2 en)))))

(defn ecc-vector
  [r v]
  (let [V-sq (Math/pow (m/mag v) 2)
        R (m/mag r)
        mu (:mu wgs84)
        a (map * (repeat (- (/ V-sq mu) (/ 1 R))) r)
        b (map * (repeat (/ (m/dot r v) mu)) v)]
    (map - a b)))

(defn angular-momentum
  [r v]
  (m/cross r v))

(defn inclination
  [r v]
  (let [H (angular-momentum r v)
        hk (last H)
        h (m/mag H)]
    (m/rad->deg (Math/acos (/ hk h)))))

(defn node-vector
  [r v]
  (let [K [0 0 1]
        H (angular-momentum r v)]
    (m/cross K H)))

(defn right-ascension
  [r v]
  (let [N (node-vector r v)
        ni (first N)
        nj (second N)
        n (m/mag N)
        i (inclination r v)]
    (if (or (zero? i) (>= i 180)) 0.0
      (let [raan (m/rad->deg (Math/acos (/ ni n)))]
        (if (neg? nj) (- 360 raan) raan)))))

(defn argument-of-perigee
  [r v]
  (let [N (node-vector r v)
        E (ecc-vector r v)
        n (m/mag N)
        e (m/mag E)
        ei (first E)
        ej (second E)
        ek (last E)
        i (inclination r v)
        a (m/dot N E)
        b (* n e)]
    (cond
      (zero? e) 0.0
      (or (zero? i) (>= i 180)) (m/rad->deg (Math/atan2 ej ei))
      :else (let [aop (m/rad->deg (Math/acos (/ a b)))]
              (if (neg? ek) (- 360 aop) aop)))))

(defn argument-of-latitude
  [r v]
  (let [n (node-vector r v)
        n-mag (m/mag n)
        r-mag (m/mag r)
        n-dot-v (m/dot n v)
        a (m/dot n r)
        b (* n-mag r-mag)
        aol (m/rad->deg (Math/acos (/ a b)))]
    (if (pos? n-dot-v) (- 360 aol) aol)))

(defn true-longitude
  [r v]
  (let [ri (first r)
        vi (first v)
        rm (m/mag r)
        tl (m/rad->deg (Math/acos (/ ri rm)))]
    (if (pos? vi) (- 360 tl) tl)))

(defn true-anomaly
  [r v]
  (let [e-vec (ecc-vector r v)
        e-mag (m/mag e-vec)
        r-mag (m/mag r)
        r-dot-v (m/dot r v)
        i (inclination r v)
        arg-lat? (zero? e-mag)
        tru-lon? (and (zero? e-mag) (or (zero? i) (>= i 180)))
        a (m/dot e-vec r)
        b (* e-mag r-mag)]
    (cond
      tru-lon? (true-longitude r v)
      arg-lat? (argument-of-latitude r v)
      :else (let [ta (m/rad->deg (Math/acos (/ a b)))]
              (if (neg? r-dot-v) (- 360 ta) ta)))))

;; Propagation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn two-body
  [{:keys [t a e i o w v]} prop-date]
  (let [v-fut (true-anomaly-future a e v t prop-date)]
    {:t prop-date
     :a a :e e :i i :o o :w w
     :v v-fut}))
