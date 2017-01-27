(in-ns 'image-processing.core)

;;
;; Define the filter function template Below!!!
;;



;;
;; This Function converts an RGB image into a Greyscale image
;;
(defn greyscale [image-name]
  (let [filename-in  image-name
        filename-out (apply str [ (first(str/split filename-in #"\.")) "-greyscale.png"] )
        test-image   (read-image filename-in)
        image-width  (get-width test-image)
        image-height (get-height test-image)
        new-image (new-image image-width image-height)]

    (println "image read is" filename-in "size " image-width "X" image-height)                                         ;report file read in

    (dotimes [x image-width]                                                                                           ;convert to rey scale
      (dotimes [y image-height]
        (let [rgb  (get-rgb test-image x y)
              grey (int (/ (reduce + rgb) 3.0))]
          (set-grey test-image x y grey)
        )
     )
    )

    ; save the modified test image
    (save-image test-image "png" filename-out)
    ; report
    (println "image written is" filename-out
             "with dimensions" (get-width new-image)
             "X" (get-height new-image))
    )
  )


;;
;; This Function converts an RGB image into a inverted RGB image
;;
(defn invert [image-name]
  (let [filename-in  image-name
        filename-out (apply str [ (first(str/split filename-in #"\.")) "-inverted.png"] )
        test-image   (read-image filename-in)
        image-width  (get-width test-image)
        image-height (get-height test-image)
        new-image (new-image image-width image-height)]

    (println "image read is" filename-in "size " image-width "X" image-height)                                         ;report file read in

    (dotimes [x image-width]                                                                                           ;convert to rey scale
      (dotimes [y image-height]
        (let [rgb   (get-rgb test-image x y)
              grey  (int (/ (reduce + rgb) 3.0))
              red   (first rgb)
              green (first(rest rgb))
              blue  (last rgb)]
          ;(set-grey test-image x y (- 255 grey))
          (set-rgb test-image x y [(- 255 red) (- 255 green) (- 255 blue)])
        )
     )
    )

    ; save the modified test image
    (save-image test-image "png" filename-out)
    ; report
    (println "image written is" filename-out
             "with dimensions" (get-width new-image)
             "X" (get-height new-image))
    )
  )




