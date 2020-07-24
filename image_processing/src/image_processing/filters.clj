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

    (dotimes [x image-width]                                                                                           ;convert to grey scale
      (dotimes [y image-height]
        (let [rgb  (get-rgb test-image x y)
              grey (int (/ (reduce + rgb) 3.0))]
          (set-grey new-image x y grey)
        )
     )
    )

    (save-image new-image "png" filename-out)                                                                        ; save the modified test image
    (println "image written is" filename-out                                                                          ; report status
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

    (dotimes [x image-width]                                                                                           ;convert to inverted color
      (dotimes [y image-height]
        (let [rgb   (get-rgb test-image x y)
              grey  (int (/ (reduce + rgb) 3.0))
              red   (first rgb)
              green (first(rest rgb))
              blue  (last rgb)]
          (set-rgb new-image x y [(- 255 red) (- 255 green) (- 255 blue)])
        )
     )
    )

    (save-image new-image "png" filename-out)                                                                        ; save the new inverted image
    (println "image written is" filename-out                                                                          ; report status
             "with dimensions" (get-width new-image)
             "X" (get-height new-image))
    )
  )


;;
;; This Function converts an RGB image into a greyscale Sobel or edge filtered image
;;
(defn sobel [image-name]
  (let [filename-in  image-name
        filename-out (apply str [ (first(str/split filename-in #"\.")) "-sobel.png"] )
        test-image   (read-image filename-in)
        image-width  (get-width test-image)
        image-height (get-height test-image)
        new-image (new-image image-width image-height)]

    (println "image read is" filename-in "size " image-width "X" image-height)                                         ;report file read in

    (doseq [y (range 1 (dec image-height))]                                                                            ;Apply horizontal and vertical Sobel operator
      (doseq [x (range 1 (dec image-width))]
        ;(println (int(/ (abs(sobel_h test-image x y))4)) )
        (set-grey new-image x y (int(Math/sqrt(+(*(int(/ (abs(sobel_h test-image x y))4)) (int(/ (abs(sobel_h test-image x y))4)))
                                            (*(int(/ (abs(sobel_v test-image x y))4)) (int(/ (abs(sobel_v test-image x y))4)))))))
        )
    )

    (save-image new-image "png" filename-out)                                                                         ; save the new edge image
    (println "image written is" filename-out                                                                          ; report status
             "with dimensions" (get-width new-image)
             "X" (get-height new-image))
  )
)


;;
;; This Function applyies a 3x3 Box Blur to an RGB image
;;
(defn boxBlur [image-name]
  (let [filename-in  image-name
        filename-out (apply str [ (first(str/split filename-in #"\.")) "-boxBlur.png"] )
        test-image   (read-image filename-in)
        image-width  (get-width test-image)
        image-height (get-height test-image)
        new-image (new-image image-width image-height)]

    (println "image read is" filename-in "size " image-width "X" image-height)                                         ;report file read in

    (doseq [y (range 1 (dec image-height))]                                                                            ;apply box blur
      (doseq [x (range 1 (dec image-width))]
        (let [topLeft (get-rgb test-image (dec x) (dec y))
              topCenter (get-rgb test-image (dec x) y)
              topRight (get-rgb test-image (dec x)(inc y))
              midLeft (get-rgb test-image x (dec y))
              currentPixel (get-rgb test-image x y)
              midRight (get-rgb test-image x(inc y))
              lowLeft (get-rgb test-image (inc x) (dec y))
              lowCenter (get-rgb test-image (inc x) y)
              lowRight (get-rgb test-image (inc x)(inc y))

              red (int(/ (+(first topLeft)(first topCenter)(first topRight)
                           (first midLeft)(first currentPixel)(first midRight)
                           (first lowLeft)(first lowCenter)(first lowRight))9))

              green (int(/ (+(first(rest topLeft))(first(rest topCenter))
                             (first(rest topRight)) (first(rest midLeft))
                             (first(rest currentPixel))(first(rest midRight))
                             (first(rest lowLeft))(first(rest lowCenter))(first(rest lowRight)))9))

              blue (int(/ (+(last topLeft)(last topCenter)(last topRight)
                            (last midLeft)(last currentPixel)(last midRight)
                            (last lowLeft)(last lowCenter)(last lowRight))9))

              ]

          (set-rgb new-image x y [red green blue])
        )
      )
    )

    (save-image new-image "png" filename-out)                                                                         ; save the new blured image
    (println "image written is" filename-out                                                                          ; report status
             "with dimensions" (get-width new-image)
             "X" (get-height new-image))
  )
)

;;
;; This Function applyies a 3x3 sharpening filter to an RGB image
;;
(defn unsharp [image-name]
  (let [filename-in  image-name
        filename-out (apply str [ (first(str/split filename-in #"\.")) "-unsharp.png"] )
        test-image   (read-image filename-in)
        image-width  (get-width test-image)
        image-height (get-height test-image)
        new-image (new-image image-width image-height)
        sharpenForce 1
;;         filter     [[0, (* -1 sharpenForce), 0]
;;                     [(* -1 sharpenForce), (+(* 9 sharpenForce)1),(* -1 sharpenForce)]
;;                     [0, (* -1 sharpenForce), 0]]
        filter     [[(/ 1 9),(/ 1 9),(/ 1 9)]
                    [(/ 1 9),(/ 1 9),(/ 1 9)]
                    [(/ 1 9),(/ 1 9),(/ 1 9)]]

        ]

    (println "image read is" filename-in "size " image-width "X" image-height)                                         ;report file read in

    (doseq [y (range 1 (dec image-height))]                                                                            ;apply box unsharpen
      (doseq [x (range 1 (dec image-width))]
        (let [topLeft (get-grey test-image (dec x) (dec y))
              topCenter (get-grey test-image (dec x) y)
              topRight (get-grey test-image (dec x)(inc y))
              midLeft (get-grey test-image x (dec y))
              currentPixel (get-grey test-image x y)
              midRight (get-grey test-image x(inc y))
              lowLeft (get-grey test-image (inc x) (dec y))
              lowCenter (get-grey test-image (inc x) y)
              lowRight (get-grey test-image (inc x)(inc y))


              grey (int (+ (*((filter 0)0)topLeft)(*((filter 0)1)topCenter)(*((filter 0)2)topRight)
                           (*((filter 1)0)midLeft)(*((filter 1)1)currentPixel)(*((filter 1)2)midRight)
                           (*((filter 2)0)lowLeft)(*((filter 2)1)lowCenter)(*((filter 2)2)lowRight)))

              newG (int (+ currentPixel(*(- currentPixel grey) sharpenForce)))


          (set-grey new-image x y newG)
        )
      )
    )

    (save-image new-image "png" filename-out)                                                                         ; save the new sharpened image
    (println "image written is" filename-out                                                                          ; report status
             "with dimensions" (get-width new-image)
             "X" (get-height new-image))
  )
)





;;
;; This Function applyies a linear sharpen filter to an RGB image
;;
(defn linearSharpen [image-name]
  (let [filename-in  image-name
        filename-out (apply str [ (first(str/split filename-in #"\.")) "-linearSharpen.png"] )
        test-image   (read-image filename-in)
        image-width  (get-width test-image)
        image-height (get-height test-image)
        new-image (new-image image-width image-height)]

    (println "image read is" filename-in "size " image-width "X" image-height)                                         ;report file read in

    (doseq [y (range 1 (dec image-height))]
      (doseq [x (range 1 (dec image-width))]
        (let [topLeft (get-rgb test-image (dec x) (dec y))
              topCenter (get-rgb test-image (dec x) y)
              topRight (get-rgb test-image (dec x)(inc y))
              midLeft (get-rgb test-image x (dec y))
              currentPixel (get-rgb test-image x y)
              midRight (get-rgb test-image x(inc y))
              lowLeft (get-rgb test-image (inc x) (dec y))
              lowCenter (get-rgb test-image (inc x) y)
              lowRight (get-rgb test-image (inc x)(inc y))
              amount 1

              red (int(/ (+(first topLeft)(first topCenter)(first topRight)
                           (first midLeft)(first currentPixel)(first midRight)
                           (first lowLeft)(first lowCenter)(first lowRight))9))
              fineR (- (first currentPixel) red)
              outR (int (+ (first currentPixel) (* amount fineR)))

              green (int(/ (+(first(rest topLeft)) (first(rest topCenter)) (first(rest topRight))
                             (first(rest midLeft)) (first(rest currentPixel)) (first(rest midRight))
                             (first(rest lowLeft)) (first(rest lowCenter)) (first(rest lowRight)))9))
              fineG (- (first(rest currentPixel)) green)
              outG (int (+ (first(rest currentPixel)) (* amount fineG)))

              blue (int(/ (+(last topLeft)(last topCenter)(last topRight)
                            (last midLeft)(last currentPixel)(last midRight)
                            (last lowLeft)(last lowCenter)(last lowRight))9))
              fineB (- (last currentPixel) blue)
              outB (int (+ (last currentPixel) (* amount fineB)))
              ]

          (set-rgb new-image x y [outR outG outB])
        )
      )
    )

    (save-image new-image "png" filename-out)                                                                         ; save the new sharpened image
    (println "image written is" filename-out                                                                          ; report status
             "with dimensions" (get-width new-image)
             "X" (get-height new-image))
  )
)

