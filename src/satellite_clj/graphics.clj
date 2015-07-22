(ns satellite-clj.graphics
  (:require [satellite-clj.coverage :refer [coverage-matrix]]
            [satellite-clj.math :refer [mm-add norm-matrix]]
            [satellite-clj.surface :refer [elevation]]))

;; Images ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def images
  "Default images for use in graphics panels. Options are:

     :light - equirectangular map projection containing country outlines
     :dark - equirectangular map projection containing countries (filled in)"
  {:light   "equirect_light.png"
   :dark    "equirect_dark.png"})

(defn background-image
  "Load a background image using a keyword to load a default image (see
   satellite-clj.graphics/images), or a string containing the file-path to
   the desired image.

   Example:
     (background-image :dark)
       ;=> BufferedImage@...
     (background-image \"C:\\\\User\\\\Pictures\\\\image.png\")
       ;=> BufferedImage@... (on Windows)
     (background-image \"/home/user/pictures/image.png\")
       ;=> BufferedImage@... (on Linux)"
  [selection]
  (let [img (get images selection)]
    (if (nil? img)
      (javax.imageio.ImageIO/read (clojure.java.io/file selection))
      (javax.imageio.ImageIO/read (clojure.java.io/resource img)))))

(defn save-image!
  "Save a panel as a PNG image. The panel must be visible when save-image! is
  executed (see satellite-clj.graphics/show-panel!). Optionally takes a File
  argument as an output location, default output location is in the user's
  home directory.

  Example:
    (def coverage-panel (build-panel :coverage))
    (show-panel! coverage-panel)

    (save-image! coverage-panel)
      ;=> <output PNG to home directory>
    (save-image! coverage-panel \"C:\\\\User\\\\Pictures\\\\image.png\")
      ;=> <output PNG, named image.png, to Pictures directory>"
  ([[panel props-atom image-atom]]
    (let [path (System/getProperty "user.home")
          name (format "img_%06x.png" (rand-int (Math/pow 2 24)))]
      (save-image! [panel props-atom image-atom]
                   (clojure.java.io/file path name))))
  ([[panel props-atom image-atom] output-file]
    (let [w (.getWidth panel)
          h (.getHeight panel)
          out-image (java.awt.image.BufferedImage.
                      w h java.awt.image.BufferedImage/TYPE_INT_ARGB)
          image-graphics (.createGraphics out-image)]
      (.paint panel image-graphics)
      (javax.imageio.ImageIO/write out-image "png" output-file))))

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

;; Graphics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-overlay
  [matrix alpha]
  (let [width (count (first matrix))
        height (count matrix)
        overlay (java.awt.image.BufferedImage.
                  width height java.awt.image.BufferedImage/TYPE_4BYTE_ABGR)]
    (doseq [y (range height) x (range width)]
      (.setRGB overlay x y (val->rgba alpha (get-in matrix [y x]))))
    overlay))

(defn merge-images
  "Scale and merge images onto a given canvas"
  [graphics width height images]
  (.setColor graphics java.awt.Color/WHITE)
  (.fillRect graphics 0 0 width height)
  (doseq [img images]
    (.drawImage ^java.awt.Graphics2D graphics img 0 0 width height nil)))

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
  [graphics properties]
  (let [w (* 2 (:resolution properties))
        h (:resolution properties)
        a (:alpha properties)
        c-mat (norm-matrix (coverage-matrix h (:locations properties)))]
    (create-overlay c-mat a)))

(defn draw-coverage
  [graphics width height props-atom image-atom]
  (let [defaults {:alpha 0.6 :locations [] :resolution 180 :image :light}
        properties (merge defaults @props-atom)
        cov-img (background-image (:image properties))]
    (when (nil? @image-atom)
      (reset! image-atom (coverage-overlay graphics properties)))
    (.clearRect ^java.awt.Graphics2D graphics 0 0 width height)
    (merge-images graphics width height [cov-img @image-atom])))

;; Visibility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn visibility-matrix
  [resolution minimum-elevation ground altitude]
  (let [lat-scale #(+ (* % (/ 180 (- (int resolution)))) 90)
        lon-scale #(- (* % (/ 360 (* 2 (int resolution)))) 180)
        lats (map lat-scale (range resolution))
        lons (map lon-scale (range (* 2 resolution)))
        vis-fn (fn [p] (if (>= (elevation ground p) minimum-elevation) 1 0))]
    (for [lat lats] (map #(vis-fn [lat % altitude]) lons))))

(defn visibility-overlay
  "Update overlay image for visibility panel."
  [graphics properties]
  (let [max-alt 36000
        res (:resolution properties)
        min-el (:minimum-elevation properties)
        loc (:location properties)
        alpha (:alpha properties)
        steps (range 0 max-alt (/ max-alt (:steps properties)))
        mats (map #(visibility-matrix res min-el loc %) steps)
        v-mat (norm-matrix (reduce mm-add mats))]
    (create-overlay v-mat alpha)))

(defn draw-visibility
  [graphics width height props-atom image-atom]
  (let [defaults {:alpha 0.6 :location [0 0 0] :minimum-elevation 0 :steps 72
                  :resolution 180 :image :light}
        properties (merge defaults @props-atom)
        vis-img (background-image (:image properties))]
    (when (nil? @image-atom)
      (reset! image-atom (visibility-overlay graphics properties)))
    (.clearRect ^java.awt.Graphics2D graphics 0 0 width height)
    (merge-images graphics width height [vis-img @image-atom])))

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
  {:coverage draw-coverage
   :visibility draw-visibility})

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
