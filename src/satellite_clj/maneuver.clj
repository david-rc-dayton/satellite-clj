(ns satellite-clj.maneuver
  (:require [satellite-clj.math :as m]
            [satellite-clj.properties :refer [wgs84 g-accel]]))

(defn max-delta-v
  [isp wet-mass dry-mass]
  (* isp g-accel (Math/log (/ wet-mass dry-mass))))

(defn transfer-axis
  [r1 r2]
  (/ (+ r1 r2) 2))

(defn velocity
  [position semi-major-axis]
  (let [mu (:mu wgs84)
        R (double position)
        a (double semi-major-axis)]
    (Math/sqrt (* mu (- (/ 2 R) (/ 1 a))))))

(defn angular-velocity
  [position]
  (Math/sqrt (/ (:mu wgs84) (Math/pow position 3))))

(defn tof-hohmann
  [transfer-axis]
  (let [mu (:mu wgs84)
        pi Math/PI
        at (double transfer-axis)]
    (* pi (Math/sqrt (/ (* at at at) mu)))))

(defn hohmann-transfer
  [r1 r2]
  (let [a-t (transfer-axis r1 r2)
        v-1 (velocity r1 r1)
        v-t1 (velocity r1 a-t)
        v-t2 (velocity r2 a-t)
        v-2 (velocity r2 r2)
        dv-1 (- v-t1 v-1)
        dv-2 (- v-2 v-t2)
        dv-total (+ (Math/abs dv-1) (Math/abs dv-2))
        tof (tof-hohmann a-t)]
    {:burn [dv-1 dv-2] :delta-v dv-total :tof tof}))

(defn plane-change
  [position semi-major-axis i1 i2]
  (let [v (velocity position semi-major-axis)
        theta (m/deg->rad (Math/abs (- i2 i1)))]
    (* 2 v (Math/sin (/ theta 2)))))

(defn coorbital-wait-time
  [r1 r2 phi]
  (let [a-t (transfer-axis r1 r2)
        tof (tof-hohmann a-t)
        w-i (angular-velocity r1)
        w-t (angular-velocity r2)
        a-lead (* w-t tof)
        phi-initial (m/deg->rad phi)
        phi-final (- Math/PI a-lead)]
    (/ (- phi-final phi-initial) (- w-t w-i))))
