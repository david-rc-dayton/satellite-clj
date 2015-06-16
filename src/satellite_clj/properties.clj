(ns satellite-clj.properties
  "Physical properties and units used in calculation.")

(def grav 6.67259e-20)

(def wgs84
  (let [semi-major-axis 6378.137
        inverse-flattening 298.257223563
        semi-minor-axis (* semi-major-axis (- 1 (/ 1 inverse-flattening)))
        mean-radius (/ (+ (* 2 semi-major-axis) semi-minor-axis) 3)
        first-eccentricity-squared (- 1 (* (/ semi-minor-axis semi-major-axis)
                                           (/ semi-minor-axis semi-major-axis)))
        gravatational-parameter 398600.4418]
    {:a semi-major-axis
     :b semi-minor-axis
     :r mean-radius
     :e2 first-eccentricity-squared
     :mu gravatational-parameter}))
