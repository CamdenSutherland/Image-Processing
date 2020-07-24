(ns image-processing.core
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File]))

(require '[clojure.string :as str])


;;
;; Absolute value method: Return the absolute value of a given input
;;
(defn abs [n] (max n (- n)))


;;
;; Create a new image
;;
(defn new-image
  "Function to create a new image."
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_RGB))


;;
;; Get the width of an image
;;
(defn get-width
  "Function to get the width of an image."
  [image]
  (.getWidth image))

;;
;; Get the height of an image
;;
(defn get-height
  "Function to get the height of an image."
  [image]
  (.getHeight image))


;;
;; Read in an image from file
;;
(defn read-image
  "Function to read an image from a file."
  [filename]
    (let [file (File. filename)]
      (ImageIO/read file)
    ))


;;
;; Save image to file
;;
(defn save-image
  "Function to save an image with a particular extension to a file."
  [image extension filename]
    (let [file (File. filename)]
      (ImageIO/write image extension file)
    ))


;;
;; Get RGB value for pixel at location in image
;;
(defn get-rgb
  "Function to get the RGB components of a pixel in a vector of length 3."
  [image x y]
    (let [rgb (.getRGB image x y)
          red (bit-shift-right (bit-and rgb 0xFF0000) 16)
          green (bit-shift-right (bit-and rgb 0xFF00) 8)
          blue (bit-and rgb 0xFF)
          ]
        (vec (list red green blue))
      ))


;;
;; Get grey value for pixel at location in image
;;
(defn get-grey
  "Function to get the grey value of a pixel"
  [image x y]
    (let [rgb (.getRGB image x y)
          red (bit-shift-right (bit-and rgb 0xFF0000) 16)
          green (bit-shift-right (bit-and rgb 0xFF00) 8)
          blue (bit-and rgb 0xFF)]
        (int (/ (reduce + (vec (list red green blue))) 3.0))
          ;(vec (list red))
      ))


;;
;; Set RGB pixel in image as given color values
;;
(defn set-rgb
  "Function to set the RGB components of a pixel."
  [image x y [red green blue]]
     (let [rgb (+ (bit-shift-left red 16)
                  (bit-shift-left green 8)
                  (bit-shift-left blue 0) )]
       (.setRGB image x y rgb)
   ))



;;
;; Set Greyscale pixel in image as given color values
;;
(defn set-grey
  "Function to set the grey value of a pixel."
  [image x y grey]
   (set-rgb image x y [grey grey grey])
)


;;
;; Horizontal sobel method: Return the output of the horizontal sobel filter on a pixel in an image
;;
(defn sobel_h [image x y]

  (get-grey image x y)
  (+(*(get-grey image (dec x) (dec y)) -1) (*(get-grey image (dec x) y) 0) (*(get-grey image (dec x)(inc y)) 1)
    (*(get-grey image x (dec y)) -2)       (*(get-grey image x y) 0)       (*(get-grey image x(inc y)) 2)
    (*(get-grey image (inc x) (dec y)) -1) (*(get-grey image (inc x) y) 0) (*(get-grey image (inc x)(inc y)) 1))
  )


;;
;; Vertical sobel method: Return the output of the vetical sobel filter on a pixel in an image
;;
(defn sobel_v [image x y]

  (get-grey image x y)
  (+(*(get-grey image (dec x) (dec y)) 1)   (*(get-grey image (dec x) y) 2)   (*(get-grey image (dec x)(inc y)) 1)
    (*(get-grey image x (dec y)) 0)         (*(get-grey image x y) 0)         (*(get-grey image x(inc y)) 0)
    (*(get-grey image (inc x) (dec y)) -1)  (*(get-grey image (inc x) y) -2)  (*(get-grey image (inc x)(inc y)) -1))
  )



;;
;; Load filters file
;;
(load "filters")

;;
;; Main method
;;
(defn -main
  "Displays all the image processes"
  [& args]
  (println "Processing Image:")
  (greyscale "kodim20.png")
  (invert "kodim20.png")
  (sobel "kodim20.png")
  (boxBlur "kodim20.png")
  (unsharp "kodim20.png")
  (linearSharpen "kodim20.png")
  )
