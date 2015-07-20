(ns satellite-clj.coverage
  "Matrix generation functions for satellite coverage analysis. Operations work
   primarily in radians.")

(defn haversine
  [phi1 lam1 phi2 lam2]
  (let [phi1 (double phi1)
        lam1 (double lam1)
        phi2 (double phi2)
        lam2 (double lam2)
        delta-phi (Math/pow (Math/sin (/ (- phi2 phi1) 2)) 2)
        delta-lam (Math/pow (Math/sin (/ (- lam2 lam1) 2)) 2)
        a (+ delta-phi (* (Math/cos phi1) (Math/cos phi2) delta-lam))]
    (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))))

(defn horizon
  [alt]
  (let [alt (double alt)
        r 6371]
    (Math/acos (/ r (+ r alt)))))

(defn view-fn
  [lat lon alt]
  (let [lim (horizon alt)
        phi (Math/toRadians (double lat))
        lam (Math/toRadians (double lon))]
    (fn [p l] (if (<= (double (haversine phi lam p l)) (double lim)) 1 0))))

(defn blank-matrix
  [y x]
  (make-array Integer/TYPE y x))

(defn coverage-matrix
  [res coords]
  (binding [*unchecked-math* true]
    (let [y (int res)
          x (int (* 2 res))
          m (blank-matrix y x)
          vfns (map #(apply view-fn %) coords)
          pm (/ Math/PI (- y))
          ps (/ Math/PI 2)
          lm (/ (* 2 Math/PI) x)
          ls Math/PI
          p-scale #(+ (* % pm) ps)
          l-scale #(- (* % lm) ls)]
      (doseq [yc (range 0 y) xc (range 0 x)]
        (let [p (p-scale yc)
              l (l-scale xc)
              v (apply + (map #(% p l) vfns))]
          (aset-int m yc xc v)))
      m)))

(defn print-matrix
  [m]
  (let [height (int (alength m))
        width (int (alength (first m)))]
    (doseq [y (range height) x (range width)]
      (if (= x (dec width))
        (println (aget m y x))
        (print (aget m y x))))))
