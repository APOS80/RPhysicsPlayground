#lang racket


(require racket/gui/base)
;(require racket/include)
;(include "Gmath.rkt")
(include "mvmath.rkt")

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

(define (bounce v n) ;v velosity vector, n normal vector, wall bounce type
  (let* ([u (vectorScalarMult (/(vectorDotProduct v n)
                                (vectorDotProduct n n)) n)] ;Frictionless
         [w (vectorSub v u)]
         [pr (printf "v ~a u ~a w ~a\n" v u w)]
         [f 1];friction
         [r 0.8];elasticity
         [vn (vectorSub (vectorScalarMult f w)(vectorScalarMult r u))]
         [pr2 (printf "vn ~a\n" vn)])
    vn))

;(bounce2D '(-1 0) '(1 0) 100 100)

(define (bounceE v1 v2 n m1 m2) ;v velosity vector, n normal vector, bounce with momentum transfer
  (let* ([prr2 (printf "v1: ~a v2: ~a\n" v1 v2)]
         [p1 (vectorScalarMult m1 v1)]
         [p2 (vectorScalarMult m2 v2)]
         [ps (vectorAdd p1 p2)]
         [v1b (vectorScalarMult (vectorMagn (vectorScalarDiv p2 m1)) (vectorNorm v1))]
         [u (vectorScalarMult (/(vectorDotProduct v1b n)
                                (vectorDotProduct n n)) n)] ;Frictionless
         [w (vectorSub v1b u)]
         [f 0.8];friction
         [r 0.8];elasticity
         [vn (vectorSub (vectorScalarMult f w)(vectorScalarMult r u))]
         )
    vn))

(define (bounceS v1 n) ;Simplest reflection, the core, frictionless wall bounce
  (let* ([u (vectorScalarMult (* 2 (vectorDotProduct v1 n)) n)]
         [w (vectorSub v1 u)]
         [pr (printf "v ~a u ~a w ~a\n" v1 u w)]
         )
    w))

(define (attraction body otherBody) ;Creates a vector to adjust planet heading depending on all other bodies 
  (let* ([dx (- (body-px otherBody) (body-px body))]
         [dy (- (body-py otherBody) (body-py body))]
         [distance (sqrt (+ (expt dx 2) (expt dy 2)))]) ;Distance between bodys
         (directionOfForce dx dy
                           (force G (body-mass body) (body-mass otherBody) distance))))

(define (collisionDetection body otherBody)
  (let* ([dx (- (body-px otherBody) (body-px body))]
         [dy (- (body-py otherBody) (body-py body))]
         [distance (sqrt (+ (expt dx 2) (expt dy 2)))]
         [rsum (+ (body-radius body) (body-radius otherBody))])
  (if (>  rsum distance)
                 (let* ([b (bounceE (list(body-vx body)(body-vy body))
                                     (list(body-vx otherBody)(body-vy otherBody))
                                     (vectorNorm (list dx dy))
                                     (body-mass body)
                                     (body-mass otherBody))]
                        [resetV (vectorScalarMult  (+ (* (- rsum distance)
                                                        (/ (body-mass otherBody)
                                                        (+ (body-mass body)(body-mass otherBody))))distance)
                                                   (vectorNorm(list (* dx -1) (* dy -1))))]
                        [resetTo (list (+ (list-ref resetV 0) (body-px otherBody))
                                       (+ (list-ref resetV 1) (body-py otherBody)))]
                        )(append resetTo b))
                 '("n"))))

(define timestep (* 12 3600)) ;Half a day

(define (totalAttraction body bodies fxy) ;Creates a list of vectors, a vector for every body
  (if (equal? bodies '())
      fxy
      (totalAttraction body (cdr bodies) (map + fxy (attraction body (car bodies)))))
  )

(define (totalCollision body bodies col) ;Creates a list of vectors, a vector for every body
  (if (equal? bodies '())
      (remove* (list "n") col equal?)
      (totalCollision body (cdr bodies) (append col (collisionDetection body (car bodies)))))
  )

(define (gravity bodies timestep)
  (let* ([bodiesCd (for/list ([b bodies]) (totalCollision b (remove b bodies) '()))]
         [bodiesCD (for/list ([b bodies] [c bodiesCd]) (if (equal? '() c) b
                                                           (body (body-id b)
                                                                 (car c)
                                                                 (car(cdr c))
                                                                 (car(cdr(cdr c)))
                                                                 (car(cdr (cdr(cdr c))))
                                                                 (body-mass b)
                                                                 (body-radius b)
                                                                 (body-color b)) ))]
         [forces (for/list ([b bodiesCD]) (totalAttraction b (remove b bodiesCD) '(0 0)))]
         
         [vectors (for/list ([f forces][b bodiesCD]) (list (+ (body-vx b) (*(/ (car f) (body-mass b)) timestep))
                                                         (+ (body-vy b) (* (/(car(cdr f)) (body-mass b)) timestep))))]
         
         [positions (for/list ([v vectors][b bodiesCD]) (list (+ (body-px b) (* (car v) timestep))
                                                            (+ (body-py b) (* (car (cdr v)) timestep))))])

    (for/list ([b bodiesCD][v vectors][p positions])
      (body (body-id b)
            (car p) ;px
            (car(cdr p)) ;py
            (car v) ;vx
            (car(cdr v)) ;vy
            (body-mass b)
            (body-radius b)
            (body-color b)))
    ))
;(struct body (id px py vx vy mass radius color)) ;just a reminder of the struct


;A list of bodies, size of planets is not real... you woldent se the planets. 
(define testCollPlanets (list
                         ;(body "Sun" 0 0 0 0 (* 1.98892 (expt 10 30)) (* 0.15 AU) "yellow")
                         ;(body "Mercury" (* -0.387098 AU) 0 0 (* -47.362 1000) (* 3.3011 (expt 10 23)) (* 0.01 AU) "red")
                         ;(body "Venus" (* 0.723 AU) 0 0 (* 35.02 1000) (* 4.8685 (expt 10 24)) (* 0.02 AU) "brown")
                         ;(body "Earth" (* -1 AU) 0 0 (* -29.783 1000) (* 5.9742 (expt 10 24)) (* 0.02 AU) "green")
                         ;(body "Mars" (* -1.5236 AU) 0 0 (* -24.077 1000) (* 6.4174 (expt 10 23)) (* 0.015 AU) "orange")
                         (body "Havoc" (* -0.5 AU) 0 (* 3 1000) 0 (* 10 (expt 10 24)) (* 0.05 AU) "green")
                         (body "Midget" (* 0.5 AU) 0  (* -7 1000) (* 0.5 1000) (* 10 (expt 10 24)) (* 0.05 AU) "blue")
                         ))

;A gui below
(define myframe (new frame%
                     [width 100]
                     [height 100]
                     [label "Solar system simulator"]))

(define (solarPainter bodies timestep scale);Update planet positions and paint
    (let ([ bp (gravity bodies timestep)])
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
                       (solarPainter testCollPlanets timestep SCALE))] ;Call the planetpainter
                  ))


(define refreshTimer ;Canvas refresh
  (new timer% [notify-callback (lambda () (send my_canvas refresh))]))

(define dc (send my_canvas get-dc))

(send myframe show #t)
(send refreshTimer start 16 #f) ;Start refresh timer

