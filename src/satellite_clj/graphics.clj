(ns satellite-clj.graphics
  (:require [satellite-clj.coverage :refer [coverage-matrix]]
            [satellite-clj.math :refer [norm-matrix]]))

;; Images ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def images
  {:light   "equirect_light.png"
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
        x (* c (- 1 (Math/abs (double (dec (mod h-prime 2))))))
        rgb (cond
              (and (<= 0 h-prime) (< h-prime 1)) [c x 0]
              (and (<= 1 h-prime) (< h-prime 2)) [x c 0]
              (and (<= 2 h-prime) (< h-prime 3)) [0 c x]
              (and (<= 3 h-prime) (< h-prime 4)) [0 x c]
              (and (<= 4 h-prime) (< h-prime 5)) [x 0 c]
              (and (<= 5 h-prime) (< h-prime 6)) [c 0 x]
              :else                              [0 0 0])]
    (java.awt.Color. (float (nth rgb 0))
                     (float (nth rgb 1))
                     (float (nth rgb 2)) a)))

(defn val->rgba
  [a x]
  (.getRGB ^java.awt.Color (hsva->rgba (* x 240) 1 1 a)))

;; JPanel  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn merge-images
  "Scale and merge images onto a given canvas"
  [graphics width height images]
  (doseq [img images]
    (.drawImage ^java.awt.Graphics2D graphics img 0 0 width height nil nil)))

(defn update-panel!
  "Update a panel's properties with new arguments."
  [[panel props-atom image-atom] & args]
  (let [panel ^javax.swing.JPanel panel]
    (swap! props-atom merge (apply hash-map args))
    (reset! image-atom nil)
    (.repaint panel 0 0 (.getWidth panel) (.getHeight panel))))

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
  (let [defaults {:alpha 0.6 :locations [] :resolution 180 :image :light}
        properties (merge defaults @props-atom)
        cov-img (coverage-image (:image properties))]
    (when (nil? @image-atom)
      (reset! image-atom (coverage-overlay graphics width height properties)))
    (.clearRect ^java.awt.Graphics2D graphics 0 0 width height)
    (merge-images graphics width height [cov-img @image-atom])))

;; Panels ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn jpanel
  [draw-fn props-atom image-atom]
  (let [panel (proxy [javax.swing.JPanel] []
                (paint [^java.awt.Graphics2D graphics]
                  (let [width (proxy-super getWidth)
                        height (proxy-super getHeight)]
                    (doto graphics
                      (.setRenderingHint
                        java.awt.RenderingHints/KEY_INTERPOLATION
                        java.awt.RenderingHints/VALUE_INTERPOLATION_BICUBIC)
                      (.setRenderingHint
                        java.awt.RenderingHints/KEY_RENDERING
                        java.awt.RenderingHints/VALUE_RENDER_QUALITY)
                      (.setRenderingHint
                        java.awt.RenderingHints/KEY_COLOR_RENDERING
                        java.awt.RenderingHints/VALUE_COLOR_RENDER_QUALITY)
                      (.setRenderingHint
                        java.awt.RenderingHints/KEY_STROKE_CONTROL
                        java.awt.RenderingHints/VALUE_STROKE_PURE)
                      (.setRenderingHint
                        java.awt.RenderingHints/KEY_ANTIALIASING
                        java.awt.RenderingHints/VALUE_ANTIALIAS_ON))
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
  [[panel props image] & opts]
  (let [defaults {:title "satellite-clj" :width 1000 :height 500 :on-top? false}
        properties (merge defaults (apply hash-map opts))
        frame (doto (javax.swing.JFrame.)
                (.setDefaultCloseOperation javax.swing.JFrame/DISPOSE_ON_CLOSE)
                (.setTitle (:title properties))
                (.setAlwaysOnTop (:on-top? properties))
                (.setSize (:width properties) (:height properties))
                (.setContentPane panel) (.setLocationRelativeTo nil))]
    (javax.swing.SwingUtilities/invokeLater #(.show frame))))

;; DEBUG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rand-locs
  [n]
  (let [rl (fn [] [(- (* (rand) 180) 90)
                   (- (* (rand) 360) 180)
                   (+ (* (rand) 4000) 400)])]
    (repeatedly n #(rl))))
