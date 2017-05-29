#lang racket


(require racket/gui/base)
;(require racket/include)
(include "Gmath.rkt")

; The gravitational constant G
(define G 6.67428e-11)

; Assumed scale: 100 pixels = 1AU
(define AU 149597870700) ;Astronomic Unit (m)
(define SCALE (/ 250 AU))

(struct body (id px py vx vy mass radius color) #:mutable #:transparent) ;Structure of body
;position in m, vector in m/s, mass in kg, radius

(define (force g mass otherMass distance) ;Calculate the force of attraction
  (/ (* g (* mass otherMass)) (expt distance 2)))

(define (directionOfForce dx dy force) ;Calculate direction of the force
  (let ([theta (atan dy dx)])
    (list (* (cos theta) force) (* (sin theta) force))))

(define (bounce v n) ;v velosity vector, n normal vector
  (let* ([Vfl (vectorAdd (dotProduct n (dotProduct (dotProduct v n) '(-2 -2))) v)] ;Frictionless
         [Vf  (dotProduct Vfl '(0.8 0.8))]) ;With friction
    Vf))

;(setVectorLength vector1 length1)

(define (attraction body otherBody action) ;Creates a vector to adjust planet heading depending on all other bodies 
  (let* ([dx (- (body-px otherBody) (body-px body))]
         [dy (- (body-py otherBody) (body-py body))]
         [distance (sqrt (+ (expt dx 2) (expt dy 2)))]
         [rsum (+ (body-radius body) (body-radius otherBody))]) ;Distance between bodys
    (if (>  rsum distance)
        (case action
          [("p") (printf "Hitt! ~a ~a\n" (body-id body) (body-id otherBody))]
          [("b") (let* ([b (bounce (list(body-vx body)(body-vy body))
                                   (normalizeVector (list dx dy)))];Calculate bounce angle and speed
                        [resetV (setVectorLength (list (* dx -1) (* dy -1)) (+ (* (- rsum distance)
                                                                                  (/ (body-mass otherBody)
                                                                                     (+ (body-mass body)(body-mass otherBody))))
                                                                               distance))]
                        [resetTo (list (+ (list-ref resetV 0) (body-px otherBody))
                                       (+ (list-ref resetV 1) (body-py otherBody)))]
                        )
                   (set-body-px! (list-ref testCollPlanets (index-of testCollPlanets body)) (list-ref resetTo 0))
                   (set-body-py! (list-ref testCollPlanets (index-of testCollPlanets body)) (list-ref resetTo 1))
                   (set-body-vx! (list-ref testCollPlanets (index-of testCollPlanets body)) (list-ref b 0))
                   (set-body-vy! (list-ref testCollPlanets (index-of testCollPlanets body)) (list-ref b 1)))]
          [else null])
        null)
        (directionOfForce dx dy
                        (force G (body-mass body) (body-mass otherBody) distance))))

(define timestep (* 6 3600)) ;Half a day

(define (totalAttraction body bodies fxy action) ;Creates a list of vectors, a vector for every body
  (if (equal? bodies '())
      fxy
      (totalAttraction body (cdr bodies) (map + fxy (attraction body (car bodies) action)) action))
  )

(define (gravity bodies timestep action)
  (let* ([forces (for/list ([b bodies]) (totalAttraction b (remove b bodies) '(0 0) action))]
         [vectors (for/list ([f forces][b bodies]) (list (+ (body-vx b) (*(/ (car f) (body-mass b)) timestep))
                                                         (+ (body-vy b) (* (/(car(cdr f)) (body-mass b)) timestep))))]
         [positions (for/list ([v vectors][b bodies]) (list (+ (body-px b) (* (car v) timestep))
                                                            (+ (body-py b) (* (car (cdr v)) timestep))))])

    (for/list ([b bodies][v vectors][p positions])
      (body (body-id b) (car p) (car(cdr p)) (car v) (car(cdr v))
            (body-mass b) (body-radius b) (body-color b)))
    ))
;(struct body (id px py vx vy mass radius color)) ;just a reminder of the struct


;A list of bodies, size of planets is not real... you woldent se the planets. 
(define testCollPlanets (list
                         (body "Sun" 0 0 0 0 (* 1.98892 (expt 10 30)) (* 0.15 AU) "yellow")
                         (body "Mercury" (* -0.387098 AU) 0 0 (* -47.362 1000) (* 3.3011 (expt 10 23)) (* 0.01 AU) "red")
                         (body "Venus" (* 0.723 AU) 0 0 (* 35.02 1000) (* 4.8685 (expt 10 24)) (* 0.02 AU) "brown")
                         (body "Earth" (* -1 AU) 0 0 (* -29.783 1000) (* 5.9742 (expt 10 24)) (* 0.02 AU) "green")
                         (body "Mars" (* -1.5236 AU) 0 0 (* -24.077 1000) (* 6.4174 (expt 10 23)) (* 0.015 AU) "orange")
                         (body "Havoc" 0 (* -0.5 AU) (* 30 1000) 0 (* 0.5 (expt 10 30)) (* 0.05 AU) "green")
                         ;(body "Midget" (* 0.5 AU) 0 (* -30 1000) 0 (* 5.9742 (expt 10 24)) (* 0.02 AU) "blue")
                         ))



(define (printBodies bodies scale) ;To print the numbers for control
  (if (equal? bodies '())
      (printf "Done\n")
      (let
      ([ p (printf "Position XY ~a \n" (list (body-id (car bodies))
                                             (* (body-px (car bodies)) scale)
                                             (* (body-py (car bodies)) scale)
                                             (* (body-vx (car bodies)) scale)
                                             (* (body-vy (car bodies)) scale)))])
      (printBodies (cdr bodies) scale))))

(define (loop grav bodies timestep scale n action);A numeric simulation
  (printBodies bodies scale)
  (if (> n 0)
      (loop grav (gravity bodies timestep action) timestep scale (- n 1) action)
      (printf "End")
      ))

;(loop G testCollPlanets timestep SCALE 90 printH)

;A gui below
(define myframe (new frame%
                     [width 100]
                     [height 100]
                     [label "Solar system simulator"]))

(define (solarPainter grav bodies timestep scale action);Update planet positions and paint
    (let ([ bp (gravity bodies timestep action)])
      (for ([b bp][i (length bodies)])
        ;mutate struct
        (set-body-px! (list-ref testCollPlanets i) (body-px b))
        (set-body-py! (list-ref testCollPlanets i) (body-py b))
        (set-body-vx! (list-ref testCollPlanets i) (body-vx b))
        (set-body-vy! (list-ref testCollPlanets i) (body-vy b))
        ;paint
        (send dc set-brush (make-object brush% (body-color b) 'solid))
        (send dc draw-ellipse
              (+ (* (- (body-px b) (body-radius b)) scale) 500)
              (+ (* (- (body-py b) (body-radius b)) scale) 500)
              (* (* (body-radius b) 2) scale)
              (* (* (body-radius b) 2) scale))
        )))
  

(define my_canvas (new canvas% ;Only a canvas
                 [parent myframe]
                  [min-width 1000]
                  [min-height 1000]
                  [paint-callback 
                     (lambda(canvas dc)
                       (send dc set-smoothing 'smoothed)
                       (send dc erase)
                       (send dc set-brush (make-object brush% "black" 'solid))
                       (send dc draw-rectangle 0 0 1000 1000)
                       (send dc set-alpha 1)
                       (solarPainter G testCollPlanets timestep SCALE "b"))] ;Call the planetpainter
                  ))


(define refreshTimer ;Canvas refresh
  (new timer% [notify-callback (lambda () (send my_canvas refresh))]))

(define dc (send my_canvas get-dc))

(send myframe show #t)
(send refreshTimer start 16 #f) ;Start refresh timer

