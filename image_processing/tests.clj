(use 'clojure.test)
(load "core")
;;(load "filters")

(deftest greyscale-test
  (println " ------------------------------------------------------------------------- \n")
  (println "\n Greyscale test")
  (println (time (greyscale "test.png")) "\n")
  )

(deftest invert-test
  (println " ------------------------------------------------------------------------- \n")
  (println "\n Invert test")
  (println (time (invert "test.png")) "\n")
  )

(do
  (greyscale-test)

  (invert-test)
  )
