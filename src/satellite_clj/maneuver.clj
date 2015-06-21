(ns satellite-clj.maneuver
  (:require [satellite-clj.properties :refer [wgs84]]))

(defn transfer-axis
  [r1 r2]
  (/ (+ r1 r2) 2))

(defn velocity
  [position semi-major-axis]
  (let [mu (:mu wgs84)
        R (double position)
        a (double semi-major-axis)]
    (Math/sqrt (* mu (- (/ 2 R) (/ 1 a))))))

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
