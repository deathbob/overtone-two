(ns overtone-two.core
  (:use [overtone.live]))


(defsynth beep4 [freq 440 amp 0.45]
;  (out 0 (* amp (sin-osc [freq freq]))))
  (out 0 (* amp (saw [freq freq]))))

(defsynth beep5 [freq 440 amp 0.16]
  (out 0 (distort (* 0.2 (sin-osc freq))))
  (out 1 (* 0.1 (lf-pulse:ar freq)))
  )


;lf-pulse:ar

(defn majbeep [freq]
  (beep4 freq)
  (beep4 (+ (* (/ 4 12) freq) freq))
  (beep4 (+ (* (/ 7 12) freq) freq))
)

(defn mcf[freq]
   (map #(+ (* % freq) freq) [0 (/ 4 12) (/ 7 12)]))




(defn minbeep [freq]
  (beep4 freq)
  (beep4 (+ (* (/ 3 12) freq) freq))
  (beep4 (+ (* (/ 7 12) freq) freq))
)

(defn doomfeel[freq]
  (let [less-sixth (- freq (/ freq 6))]
  (beep4 freq)
  (beep4 (+ (* (/ 4 12) freq) freq))
  (beep4 (+ (* (/ 7 12) freq) freq))

  (beep4 less-sixth)
  (beep4 (+ (* (/ 4 12) less-sixth) less-sixth))
  (beep4 (+ (* (/ 7 12) less-sixth) less-sixth))
))

(defn dimbeep [freq]
  (beep4 freq)
  (beep4 (+ (* (/ 3 12) freq) freq))
  (beep4 (+ (* (/ 6 12) freq) freq))
)

;(majbeep 120)
;(majbeep 100)
;696.666

(defn permaj[freq]
  (beep4 freq)
  (beep4 (* freq 1.5)))

(defn powerchord[freq]
  (beep4 freq)
  (beep4 (* freq 1.5))
  (beep4 (* freq 2)))

(defn trumaj[freq]
  (beep4 freq)
  (beep4 (* freq (/ 5 4)))
  (beep4 (* freq (/ 3 2)))
  )
