(ns satellite-clj.math
  "Math functions for vector, matrix, trigonometry, and quaternion operations.")

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
  (reduce + (map * a b)))

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
  "Calculate the magnitude for a vector. Takes a vector as its only argument.

   Example:
     (mag [1 2 3 4 5]) ;=> 7.416198487095663
     (mag [-2 4]) ;=> 4.47213595499958"
  [v]
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn norm
  "Calculate and return the normalized input vector. Takes a vector as its
   only argument.

   Example:
     (norm [1 2 3 4 5]) ;=> (0.13483 0.2696 0.40451 0.5393 0.6741)
     (norm [2.5 -1.5]) ;=> (0.8574929257125441 -0.5144957554275265)"
  [v]
  (let [mag (mag v)]
    (map #(/ % mag) v)))

(defn hyp
  "Calcluate and return the length of the hypotenuse formed by the combining
   two vectors. Takes two vectors as arguments.

   Example:
     (hyp [3 0 0] [0 4 0]) ;=> 5.0
     (hyp [1 -2 3] [-4 5 -6]) ;=> 9.539392014169458"
  [a b]
  (Math/hypot (mag a) (mag b)))

(defn angle
  "Returns the angle and its conjugate between two vectors, in degrees. Takes
   two vectors as arguments.

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

(defn dist
  "Calculate the Euclidean Distance between two points. Takes two vectors as
   arguments.

   Example:
     (dist [1 0] [0 1]) ;=> 1.4142135623730951
     (dist [1 2 3] [4 5 6]) ;=> 5.196152422706632"
  [a b]
  (let [diffs (map #(- %1 %2) a b)]
    (Math/sqrt (reduce + (map #(* % %) diffs)))))

;;;; Matrix Ops ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cholesky
  "Decompose a matrix into the product of a lower triangular matrix and its
   conjugate transpose. Takes a square matrix as its only argument.

   Example:
     (cholesky [[25 0 0] [0 25 0] [0 0 25]])
       ;=> [[5.0 0.0 0.0] [0.0 5.0 0.0] [0.0 0.0 5.0]]"
  [matrix]
  (let [n (count matrix)
        A (to-array-2d matrix)
        L (make-array Double/TYPE n n)]
    (doseq [i (range n) j (range (inc i))]
      (let [s (reduce + (for [k (range j)] (* (aget L i k) (aget L j k))))]
        (aset L i j (if (= i j)
                      (Math/sqrt (- (aget A i i) s))
                      (* (/ 1.0 (aget L j j)) (- (aget A i j) s))))))
    (vec (map vec L))))

(defn ms-mult
  "Multiply a matrix by a scalar value.

   Example:
     (ms-mult [[1 2 3] [0 1 0] [9 8 7]] 2) ;=> ((2 4 6) (0 2 0) (18 16 14))"
  [m s]
  (for [row m]
    (map (partial * s) row)))

(defn mv-mult
  "Multiply a matrix by a vector. Identical to computing the dot product of each
   row in the matrix and the vector argument.

   Example:
     (mv-mult [[1 0 1] [0 1 0] [0 0 1]] [2 4 6]) ;=> (8 4 6)"
  [m v]
  (for [row m]
    (dot row v)))

;;;; Quaternion Ops ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn q-product
  "Calculate the product of two quaternions, starting with the real component,
   denoted by a vector of the form [1 i j k].

   Example:
     (q-product [1 2 3 4] [4 3 2 1]) ;=> (-12 6 24 12)"
  [p q]
  (let [dp (dot (rest p) (rest q))
        cp (cross (rest p) (rest q))
        poq (map #(* (first p) %) (rest q))
        qop (map #(* (first q) %) (rest p))
        sum (map + poq qop cp)
        st (- (* (first p) (first q)) dp)]
    (conj sum st)))

(defn q-conjugate
  "Calculate the conjugate of a quaternion, starting with the real component,
   denoted by a vector of the form [1 i j k].

   Example:
     (q-conjugate [1 2 3 4]) ;=> (1 -2 -3 -4)"
  [q]
  (let [st (first q)
        en (map #(* -1 %) (rest q))]
    (conj en st)))
