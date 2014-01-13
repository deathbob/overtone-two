(ns overtone-two.core
  (:use [overtone.live]))


(defsynth beep4 [freq 440 amp 0.45]
  (out 0 (* amp (sin-osc [freq freq]))))
;  (out 0 (* amp (saw [freq freq]))))

(defsynth beep5 [freq 440 amp 0.16]
  (out 0 (distort (* 0.2 (sin-osc freq))))
  (out 1 (* 0.1 (lf-pulse:ar freq)))
  )

(defsynth beep6 [freq 440 amp 0.45]
  (out 0 (* amp (lf-pulse:ar [freq freq])))
  )

(definst doomb [freq 440 attack 0.01 sustain 4.0 release 0.01 amp 0.5]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (sin-osc [freq freq])
     amp))

(defn grindb[notes at-times durations]
  (let [time (now)]
    (doseq [[chord offset duration] (map list notes (map #(* 1000 %) at-times) durations)]
      (at (+ time offset) (doall(map #(doomb %1 0.01 duration) chord))))))
;(grindb [(mcf 120) (mcf 100) (mcf 120) (mcf 100)] [0 3.5 8.25 11.75] [8 4.75 8 4.75])

(definst saw-wave [freq 440 attack 0.05 sustain 1.0 release 0.5 vol 0.6]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (sin-osc [freq freq])
     vol))



(def unis 1)
;(def seco (/ 7 6))
(def seco (/ 9 8))
(def thir (/ 5 4))
(def four (/ 4 3))
(def fift (/ 6 4))
(def sixt (/ 5 3))
;(def seve (/ 7 4))
(def seve (/ 15 8))
(def octa 2)

(defn I[freq]
  (map #(* freq %) [unis thir fift]))

(defn II[freq]
  (map #(* freq %) [seco four sixt]))

(defn III[freq]
  (map #(* freq %) [thir fift seve]))

(defn IV[freq]
  (map #(* freq %) [four sixt octa]))

(defn V[freq]
  (map #(* freq %) [fift seve (* octa seco)]))

(defn VI[freq]
  (map #(* freq %) [sixt octa (* octa thir)]))

(defn VII[freq]
  (map #(* freq %) [seve (* octa seco) (* octa four)]))

(defn I-again[freq]
  (map #(* freq %) [octa (* octa thir) (* octa fift)]))

;(map #(apply * 400 %) (map #(flatten %&) [1 [2 (/ 9 8)]]))

(defn bob-chords[freq]
  (map #(% freq) [I II III IV V VI VII I-again]))

(defn bob-scale[freq]
  (map #(* freq %) [unis seco thir four fift sixt seve octa]))

(defn play-scale[freq]
  (let [time (now)]
    (doseq [[note offset] (map list (bob-scale freq) (map #(* 1000 %) [0 1 2 3 4 5 6 7]))]
      (at (+ offset time) (saw-wave note)))))
;      (prn note offset))))

(defn saw-chord[notes duration]
  (doall(map #(saw-wave % 0.05 duration) notes)))
;  (prn notes))

(defn play-chord-scale[freq]
  (let [time (now)]
    (doseq [[chor offset] (map list (bob-chords freq) (map #(* 1000 %) [0 1 2 3 4 5 6 7]))]
      (at (+ offset time) (saw-chord chor)))))
;      (at (+ offset time) (saw-wave (first chor))))))



(defn majbeep [freq]
  (beep4 freq)
  (beep4 (+ (* (/ 4 12) freq) freq))
  (beep4 (+ (* (/ 7 12) freq) freq))
)

(defn mcf[freq] ; major chord for [freq] ; not really a major chord...
  ; actually some augmented thing
   (map #(* freq %)  [1 (/ 16 12) (/ 19 12)]))

(defn weird-aug[freq]
     (map #(* freq %)  [1 (/ 15 12) (/ 18 12)]))

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
  (map #(* freq %) [1 (/ 5 4) (/ 3 2)])
  )

(defn trumin[freq]
  (map #(* freq %) [1 (/ 6 5) (/ 3 2)])
  )
