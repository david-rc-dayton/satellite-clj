(ns satellite-clj.coordinates
  "Functions for working with coordinates and their transforms."
  (:require [satellite-clj.properties :refer [wgs84]]
            [satellite-clj.time :as time]))

;; Unit Conversion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deg->rad 
  "Convert from degrees to radians. For example:

     (deg->rad 180) ;=> 3.141592653589793"
  [deg]
  (* deg (/ Math/PI 180)))

(defn rad->deg
  "Convert from radians to degrees. For example:

     (rad->deg Math/PI) ;=> 180.0"
  [rad]
  (* rad (/ 180 Math/PI)))

;;;; Vector Ops ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dot
  "Calculate and return the dot product of two same-sized vectors. Takes
   two 1D lists as arguments.

   Example:
     (dot [1 2 3] [4 5 6]) ;=> 32
     (dot [-13.5 34.3] [89.6 -2.6]) ;=> -1298.78"
  [a b]
  (->> (interleave a b) (partition 2 2)
    (map #(apply * %)) (reduce +)))

(defn cross
  "Calculate and return the cross product of two vectors. Takes two 3x1 vectors
   as arguments.

   Example:
     (cross [1 2 3] [4 5 6]) ;=> (-3 6 -3)
     (cross [1.4 -4.3 12.4] [54.2 5.1 -13.6]) ;=> (-4.759 691.12 240.2)"
  [a b]
  (let [c-fn #(- (* (nth a %1) (nth b %2))
                 (* (nth a %3) (nth b %4)))
        c1 [1 2 0]
        c2 [2 0 1]]
    (map c-fn c1 c2 c2 c1)))

(defn mag
  "Calculate the magnitude for a vector. Takes a 1D vector as its only argument.

   Example:
     (mag [1 2 3 4 5]) ;=> 7.416198487095663
     (mag [-2 4]) ;=> 4.47213595499958"
  [v]
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn norm
  "Calculate and return the normalized input vector. Takes a 1D vector as its
   only argument.

   Example:
     (norm [1 2 3 4 5]) ;=> (0.13483 0.2696 0.40451 0.5393 0.6741)
     (norm [2.5 -1.5]) ;=> (0.8574929257125441 -0.5144957554275265)"
  [v]
  (let [mag (mag v)]
    (map #(/ % mag) v)))

(defn hyp
  "Calcluate and return the length of the hypotenuse formed by the combining
   two vectors. Takes two 1D vectors as arguments.

   Example:
     (hyp [3 0 0] [0 4 0]) ;=> 5.0
     (hyp [1 -2 3] [-4 5 -6]) ;=> 9.539392014169458"
  [a b]
  (Math/hypot (mag a) (mag b)))

(defn angle
  "Returns the angle and its conjugate between two vectors, in degrees. Takes
   two 1D vectors as arguments.

   Example:
     (angle [1 0 0] [0 1 0]) ;=> [90.0 270.0]
     (angle [8.5 2.0 -1.5] [13.4 52.3 45.2]) ;=> [76.0212 283.9787]"
  [a b]
  (let [n (dot a b)
        m (* (mag a) (mag b))
        theta (rad->deg (Math/acos (/ n m)))]
    [theta (- 360 theta)]))

(defn components
  "Return the axis components of a vector. Takes the radius, and the
   angle (in degrees), and optionally the fundimental plane angle (also in
   degrees) as its arguments.

   Example:
     (components 5 53.1301) ;=> [3.000000164351089 3.9999998767366773]
     (components 20 30 45) ;=> [9.9999 12.2474 12.2474]"
  ([r t]
    (let [theta (deg->rad t)
          x (* (Math/cos theta) r)
          y (* (Math/sin theta) r)]
      [x y]))
  ([r t a]
    (let [theta (deg->rad t)
          alpha (deg->rad a)
          r-prime (* r (Math/cos theta))
          x (* (Math/sin theta) r)
          y (* (Math/sin alpha) r-prime)
          z (* (Math/cos alpha) r-prime)]
      [x y z])))

;;;; Quaternion Ops ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn q-product
  [p q]
  (let [dp (dot (rest p) (rest q))
        cp (cross (rest p) (rest q))
        poq (map #(* (first p) %) (rest q))
        qop (map #(* (first q) %) (rest p))
        sum (map + poq qop cp)
        st (- (* (first p) (first q)) dp)]
    (conj sum st)))

(defn q-conjugate
  [q]
  (let [st (first q)
        en (map #(* -1 %) (rest q))]
    (conj en st)))

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
  (let [phi (deg->rad latitude)
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
        phi (deg->rad lat)
        lam (deg->rad lon)
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
        p (mag [x y])
        phi-c (Math/atan2 p z)
        rn-fn #(/ a (Math/sqrt (- 1 (* e2 (Math/sin %) (Math/sin %)))))]
    (loop [phi-n phi-c dex 0]
      (if (> dex max-iter)
        [(rad->deg phi-n) (rad->deg lam) 
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