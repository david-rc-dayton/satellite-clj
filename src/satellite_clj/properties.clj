(ns satellite-clj.properties
  "Physical properties and units used in calculation.")

(def grav 6.67259e-20)

(def wgs84
  {:semi-major-axis 6378.137
   :semi-minor-axis 6356.7523822664
   :mean-radius 6371.0087940888
   :coeff-flat 0.0033528
   :ecc-squared 0.00669435873216
   :mu 398600.50883})
