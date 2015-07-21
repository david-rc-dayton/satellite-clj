(ns satellite-clj.graphics
  (:require [satellite-clj.coverage :refer [coverage-matrix]]
            [satellite-clj.math :refer [norm-matrix]]))

;; Images ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def images
  {:terrain "equirect_terrain.png"
   :light   "equirect_light.png"
   :dark    "equirect_dark.png"})

(defn coverage-image
  [k]
  (let [img (get images k)]
    (javax.imageio.ImageIO/read (clojure.java.io/file "resources" img))))

;; Color Mapping ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hsva->rgba
  [h s v a]
  (let [h (double h)
        s (double s)
        v (double v)
        a (double a)
        c (* v s)
        h-prime (/ h 60)
        x (* c (- 1 (Math/abs (dec (mod h-prime 2)))))
        rgb (cond
              (and (<= 0 h-prime) (< h-prime 1)) [c x 0]
              (and (<= 1 h-prime) (< h-prime 2)) [x c 0]
              (and (<= 2 h-prime) (< h-prime 3)) [0 c x]
              (and (<= 3 h-prime) (< h-prime 4)) [0 x c]
              (and (<= 4 h-prime) (< h-prime 5)) [x 0 c]
              (and (<= 5 h-prime) (< h-prime 6)) [c 0 x]
              :else                              [0 0 0])
        rgba (vec (map float (conj rgb a)))]
    (java.awt.Color. (nth rgba 0) (nth rgba 1) (nth rgba 2) (nth rgba 3))))

(defn val->rgba
  [a x]
  (.getRGB (hsva->rgba (* x 240) 1 1 a)))

;; JPanel  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn merge-images
  "Scale and merge images onto a given canvas"
  [graphics width height images]
  (doseq [img images] (.drawImage graphics img 0 0 width height nil nil)))

;; Coverage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn coverage-overlay
  "Update overlay image for coverage panel."
  [graphics width height properties]
  (let [res (:resolution properties)
        locs (:locations properties)
        alpha (:alpha properties)
        ovr-img (java.awt.image.BufferedImage.
                  (* 2 res) res java.awt.image.BufferedImage/TYPE_4BYTE_ABGR)
        c-mat (norm-matrix (coverage-matrix res locs))]
    (doseq [y (range (count c-mat)) x (range (count (first c-mat)))]
      (.setRGB ovr-img x y (val->rgba alpha (get-in c-mat [y x]))))
    ovr-img))

(defn draw-coverage
  [graphics width height props-atom image-atom]
  (let [defaults {:alpha 0.6 :locations [] :resolution 180 :image :dark}
        properties (merge defaults @props-atom)
        cov-img (coverage-image (:image properties))]
    (when (nil? @image-atom)
      (reset! image-atom (coverage-overlay graphics width height properties)))
    (merge-images graphics width height [cov-img @image-atom])))

;; Panels ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn jpanel
  [draw-fn props-atom image-atom]
  (let [panel (proxy [javax.swing.JPanel] []
                (paint [^java.awt.Graphics2D graphics]
                  (let [width (proxy-super getWidth)
                        height (proxy-super getHeight)]
                    (draw-fn graphics width height props-atom image-atom))))]
    [panel props-atom image-atom]))

(def panels
  {:coverage draw-coverage})

(defn build-panel
  [k & props]
  (let [draw-fn (get panels k)
        props-atom (atom (apply hash-map props))
        image-atom (atom nil)]
    (jpanel draw-fn props-atom image-atom)))

(defn show-panel!
  [[panel props image & opts]]
  (let [defaults {:title "satellite-clj" :width 1000 :height 500 :on-top? false}
        properties (merge defaults (apply hash-map opts))]
    (doto (javax.swing.JFrame.)
      (.setDefaultCloseOperation javax.swing.JFrame/DISPOSE_ON_CLOSE)
      (.setTitle (:title properties))
      (.setAlwaysOnTop (:on-top? properties))
      (.setSize (:width properties) (:height properties))
      (.setContentPane panel) (.setLocationRelativeTo nil) .show)))

;; DEBUG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rand-locs
  [n]
  (let [rl (fn [] [(- (* (rand) 180) 90)
                   (- (* (rand) 360) 180)
                   (+ (* (rand) 4000) 400)])]
    (repeatedly n #(rl))))
