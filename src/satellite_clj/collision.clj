(ns satellite-clj.collision
  "Satellite collision operations."
  (:require [satellite-clj.math :as m]))

(def random
  "A java.util.Random object, used to generate random gaussians for collision
   space sampling."
  (java.util.Random.))

(defn gauss-vec
  "Return a vector containing random Gaussian numbers. Length of the vector is
   set with input argument n.

   Example:
     (gauss-vec 3)
       ;=> (-0.4004557003991244 0.03136797947621247 -0.3867286869872684)
     (gauss-vec 2)
       ;=> (1.0895151642329841 0.3133269685653079)"
  [n]
  (take n (repeatedly #(.nextGaussian random))))

(defn collision-probability
  "Estimate the probability of collision between two satellites, given the
   relative position and covariance matrices of both satellites in the
   Radial-Intrack-Crosstrack (RIC) frame. The Hard-Body-Radius (HBR), or the
   combined satellite radii, the Sigma value, and the number of samples to run
   are also required.

   The collision probability is estimated using a Monte-Carlo simulation over
   the event space. Using a higher number of samples (>100,000) will yield
   a more accurate result. All distance related entries must be in the
   same units.

   Example:
     (def relative-position [0 5 0])

     (def asset-covariance [[2.5e5 0 0] [0 2.5e5 0] [0 0 2.5e5]])

     (def satellite-covariance [[1.3e3 0 0] [0 1.3e3 0] [0 0 1.3e3]])

     (def combined-radius 35)

     (collision-probability
       relative-position asset-covariance satellite-covariance
       combined-radius 1 250000)  ;=> 1.08E-4"
  [rel-position cov-asset cov-satellite hbr sigma samples]
  (let [n (count (first cov-asset))
        m1 (m/cholesky (m/ms-mult cov-asset sigma))
        m2 (m/cholesky (m/ms-mult cov-satellite sigma))]
    (.setSeed random 0)
    (loop [sum 0.0 total 0.0]
      (if (< total samples)
        (let [p1 (map + rel-position (m/mv-mult m1 (gauss-vec n)))
              p2 (m/mv-mult m2 (gauss-vec n))
              dc (m/dist p1 p2)]
          (recur (+ sum (if (<= dc hbr) 1.0 0.0)) (inc total)))
        (/ sum total)))))
