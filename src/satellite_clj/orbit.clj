(ns satellite-clj.orbit
  "Functions for satellite orbit operations."
  (:require [satellite-clj.coordinates :as coord]
            [satellite-clj.properties :refer [wgs84]]
            [satellite-clj.time :as time]))

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
        v (coord/deg->rad true-anomaly)
        E (coord/rad->deg (Math/acos (/ (+ e (Math/cos v))
                                        (+ 1 (* e (Math/cos v))))))
        out [E (- 360 E)]]
    (first (sort-by #(Math/abs (- true-anomaly %)) out))))

(defn mean-anomaly
  [eccentricity true-anomaly]
  (let [e eccentricity
        v true-anomaly
        E (coord/deg->rad (eccentric-anomaly e v))]
    (coord/rad->deg (- E (* e (Math/sin E))))))

(defn mean-anomaly-future
  [semi-major-axis eccentricity true-anomaly epoch prop-date]
  (let [a semi-major-axis
        e eccentricity
        v (coord/deg->rad true-anomaly)
        Mi (coord/deg->rad (mean-anomaly e true-anomaly))
        n (mean-motion a)
        TOF (/ (- (.getTime prop-date) (.getTime epoch)) 1000)
        Mf (+ Mi (* n TOF))
        k (int (/ Mf (* 2 Math/PI)))]
    (coord/rad->deg (- Mf (* 2 Math/PI k)))))

(defn eccentric-anomaly-future
  [semi-major-axis eccentricity true-anomaly epoch prop-date]
  (let [epsilon 1e-10
        a semi-major-axis
        e eccentricity
        v true-anomaly
        M (coord/deg->rad (mean-anomaly-future a e v epoch prop-date))]
    (loop [Ef M]
      (let [En (+ M (* e (Math/sin Ef)))]
        (if (< (Math/abs (- Ef En)) epsilon)
          (coord/rad->deg En)
          (recur En))))))

(defn true-anomaly-future
  [semi-major-axis eccentricity true-anomaly epoch prop-date]
  (let [a semi-major-axis
        e eccentricity
        v true-anomaly
        E (coord/deg->rad (eccentric-anomaly-future a e v epoch prop-date))]
    (coord/rad->deg (Math/acos (/ (- (Math/cos E) e)
                                  (- 1 (* e (Math/cos E))))))))

(defn mech-energy
  [r v]
  (let [V-sq (Math/pow (coord/mag v) 2)
        R (coord/mag r)
        mu (:mu wgs84)]
    (- (/ V-sq 2) (/ mu R))))

(defn semi-major-axis
  [r v]
  (let [mu (:mu wgs84)
        en (mech-energy r v)]
    (- (/ mu (* 2 en)))))

(defn ecc-vector
  [r v]
  (let [V-sq (Math/pow (coord/mag v) 2)
        R (coord/mag r)
        mu (:mu wgs84)
        a (map * (repeat (- (/ V-sq mu) (/ 1 R))) r)
        b (map * (repeat (/ (coord/dot r v) mu)) v)]
    (map - a b)))

(defn angular-momentum
  [r v]
  (coord/cross r v))

(defn inclination
  [r v]
  (let [H (angular-momentum r v)
        hk (last H)
        h (coord/mag H)]
    (coord/rad->deg (Math/acos (/ hk h)))))

(defn node-vector
  [r v]
  (let [K [0 0 1]
        H (angular-momentum r v)]
    (coord/cross K H)))

(defn right-ascension
  [r v]
  (let [N (node-vector r v)
        ni (first N)
        nj (second N)
        n (coord/mag N)
        i (inclination r v)]
    (if (or (zero? i) (>= i 180)) 0.0
      (let [raan (coord/rad->deg (Math/acos (/ ni n)))]
        (if (neg? nj) (- 360 raan) raan)))))

(defn argument-of-perigee
  [r v]
  (let [N (node-vector r v)
        E (ecc-vector r v)
        n (coord/mag N)
        e (coord/mag E)
        ei (first E)
        ej (second E)
        ek (last E)
        i (inclination r v)
        a (coord/dot N E)
        b (* n e)]
    (cond
      (zero? e) 0.0
      (or (zero? i) (>= i 180)) (coord/rad->deg (Math/atan2 ej ei))
      :else (let [aop (coord/rad->deg (Math/acos (/ a b)))]
              (if (neg? ek) (- 360 aop) aop)))))

(defn argument-of-latitude
  [r v]
  (let [n (node-vector r v)
        n-mag (coord/mag n)
        r-mag (coord/mag r)
        n-dot-v (coord/dot n v)
        a (coord/dot n r)
        b (* n-mag r-mag)
        aol (coord/rad->deg (Math/acos (/ a b)))]
    (if (pos? n-dot-v) (- 360 aol) aol)))

(defn true-longitude
  [r v]
  (let [ri (first r)
        vi (first v)
        rm (coord/mag r)
        tl (coord/rad->deg (Math/acos (/ ri rm)))]
    (if (pos? vi) (- 360 tl) tl)))

(defn true-anomaly
  [r v]
  (let [e-vec (ecc-vector r v)
        e-mag (coord/mag e-vec)
        r-mag (coord/mag r)
        r-dot-v (coord/dot r v)
        i (inclination r v)
        arg-lat? (zero? e-mag)
        tru-lon? (and (zero? e-mag) (or (zero? i) (>= i 180)))
        a (coord/dot e-vec r)
        b (* e-mag r-mag)]
    (cond
      tru-lon? (true-longitude r v)
      arg-lat? (argument-of-latitude r v)
      :else (let [ta (coord/rad->deg (Math/acos (/ a b)))]
              (if (neg? r-dot-v) (- 360 ta) ta)))))

(defn rv->kepler
  [r v t]
  {:t t
   :a (semi-major-axis r v)
   :e (coord/mag (ecc-vector r v))
   :i (inclination r v)
   :o (right-ascension r v)
   :w (argument-of-perigee r v)
   :v (true-anomaly r v)})

(defn two-body
  [{:keys [t a e i o w v]} prop-date]
  (let [v-fut (true-anomaly-future a e v t prop-date)]
    {:t prop-date
     :a a :e e :i i :o o :w w
     :v v-fut}))
