(ns satellite-clj.surface
  "Operations applicable to the Earth and other celestial bodies."
  (:require [satellite-clj.coordinates :as coord]
            [satellite-clj.math :as m]
            [satellite-clj.properties :refer [wgs84]]))

;;;; Angular Distance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn adist-haversine
  [start-point end-point]
  (let [p1 (m/deg->rad (first start-point))
        p2 (m/deg->rad (first end-point))
        l1 (m/deg->rad (second start-point))
        l2 (m/deg->rad (second end-point))
        delta-p (- p2 p1)
        delta-l (- l2 l1)
        a (+ (Math/pow (Math/sin (/ delta-p 2)) 2)
             (* (Math/cos p1) (Math/cos p2)
                (Math/pow (Math/sin (/ delta-l 2)) 2)))]
    (m/rad->deg (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a)))))))

(defn adist-cosine
  [start-point end-point]
  (let [p1 (m/deg->rad (first start-point))
        p2 (m/deg->rad (first end-point))
        l1 (m/deg->rad (second start-point))
        l2 (m/deg->rad (second end-point))
        delta-l (- l2 l1)]
    (m/rad->deg
      (Math/acos (+ (* (Math/sin p1) (Math/sin p2))
                    (* (Math/cos p1) (Math/cos p2) (Math/cos delta-l)))))))

(defn adist-equirect
  [start-point end-point]
  (let [p1 (m/deg->rad (first start-point))
        p2 (m/deg->rad (first end-point))
        l1 (m/deg->rad (second start-point))
        l2 (m/deg->rad (second end-point))
        delta-lon (- l2 l1)
        lat-m (/ (+ p2 p1) 2)
        x (* delta-lon (Math/cos (m/deg->rad lat-m)))
        y (- p2 p1)]
    (Math/sqrt (+ (* x x) (* y y)))))

(def adist-methods
  "Map associating keywords with a method of angular distance calculation.
   Available methods are:

   > `:haversine` - *Haversine Formula* (slow)  
   > `:cosine` - *Law of Cosines* (fast)  
   > `:equirect` - *Equirectangular Approximation* (fastest)"
  {:haversine adist-haversine
   :cosine    adist-cosine
   :equirect  adist-equirect})

(defn angular-distance
  [method start-point end-point]
  (let [adist-fn (get adist-methods method)]
    (adist-fn start-point end-point)))

(defn distance-to-horizon
  [altitude]
  (let [r ((:r wgs84))]
    (m/rad->deg (Math/acos (/ r (+ r altitude))))))

(defn surface-visible?
  [method observer ground]
  (let [adist-fn (get adist-methods method)
        limit (distance-to-horizon (last observer))]
    (<= (adist-fn observer ground) limit)))

;;;; Angular Diameter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn adiam-sphere
  "Calculate the angular diameter of a sphere, given the `distance` from the
   observer to the sphere's center, and the sphere's `diameter`. Both `distance`
   and `diameter` must be in the same units.

   Returns the angular diameter of the sphere, relative to the observer, in
   degrees."
  [distance diameter]
  (m/rad->deg (* 2 (Math/asin (/ diameter (* 2 distance))))))

(defn adiam-disc
  "Calculate the angular diameter of a disc, given the `distance` from the
   observer to the disc, and the disc's `diameter`. Both `distance`
   and `diameter` must be in the same units.

   Returns the angular diameter of the disc, relative to the observer, in
   degrees."
  [distance diameter]
  (m/rad->deg (* 2 (Math/atan (/ diameter (* 2 distance))))))

(def adiam-methods
  "Map associating keywords with a method of angular diameter calculation.
   Available methods are:

   > `:sphere` - round objects  
   > `:disc` - flat objects"
  {:sphere adiam-sphere
   :disc   adiam-disc})

(defn angular-diameter
  "Calculate the angular diameter of an object as viewed from an observer. Takes
   three arguments:

   > `shape` - Keyword of the object's shape (see `adiam-methods`)  
   > `distance` - Distance from the observer to the center of the object  
   > `diameter` - Actual diameter of the object being viewed

   `distance` and `diameter` must have the same units.

   Returns the angular diameter of the object, relative to the observer, in
   degrees."
  [shape distance diameter]
  (let [adiam-fn (get adiam-methods shape)]
    (adiam-fn distance diameter)))

;;;; Look Angle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn azimuth
  [earth-station satellite]
  (let [Le (m/deg->rad (first earth-station))
        Ls (m/deg->rad (first satellite))
        le (m/deg->rad (second earth-station))
        ls (m/deg->rad (second satellite))
        ls-le (- ls le)
        y (* (Math/sin ls-le) (Math/cos Ls))
        x (- (* (Math/cos Le) (Math/sin Ls))
             (* (Math/sin Le) (Math/cos Ls) (Math/cos ls-le)))]
    (mod (+ 360 (m/rad->deg (Math/atan2 y x))) 360)))

(defn elevation
  [earth-station satellite]
  (let [A (m/deg->rad (first earth-station))
        B (m/deg->rad (first satellite))
        Lt (- (second earth-station) (second satellite))
        L (m/deg->rad (cond 
                            (> Lt 180)  (- Lt 360)
                            (< Lt -180) (+ Lt 360)
                            :else Lt))
        D (m/rad->deg
            (Math/acos (+ (* (Math/sin A) (Math/sin B))
                          (* (Math/cos A) (Math/cos B) (Math/cos L)))))
        K (/ (+ (coord/geo-radius (first satellite)) (last satellite))
             (+ (coord/geo-radius (first earth-station)) (last earth-station)))
        D-prime (m/deg->rad (- 90 D))]
    (m/rad->deg (Math/atan (- (Math/tan D-prime)
                                  (/ 1 (* K (Math/cos D-prime))))))))

(defn distance
  [earth-station satellite]
  (let [es (coord/geo->ecef earth-station)
        sat (coord/geo->ecef satellite)
        x-delta (Math/pow (- (first es) (first sat)) 2)
        y-delta (Math/pow (- (second es) (second sat)) 2)
        z-delta (Math/pow (- (last es) (last sat)) 2)]
    (Math/sqrt (+ x-delta y-delta z-delta))))

(defn look-angle
  [earth-station satellite]
  (let [az (azimuth earth-station satellite)
        el (elevation earth-station satellite)
        rng (distance earth-station satellite)
        vis? (pos? el)]
    {:az az :el el :rng rng :vis? vis?}))
