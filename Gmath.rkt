;#lang racket

;(require math)

(define (dotProduct vector1 vector2); any size 1*3 matrix and upp... (multiplikation)
           (for/list ([v1 vector1] [v2 vector2])
             (* v1 v2)))

;(dotProduct '(1 1 1) '(2 2 1))

(define (makeVector frompoint topoint) ;Make a vector
  (for/list ([f frompoint] [t topoint])
    (- f t))
  )

;(makeVector '(2 2 2) '(4 4 4))

(define (lengthPP p1 p2)
  (sqrt (+ (expt (- (list-ref p1 0)(list-ref p2 0)) 2);x
           (expt (- (list-ref p1 1)(list-ref p2 1)) 2))));y

;(normalizeVector '(5 8))

(define (normalizeVector vec)
  (let ([L (lengthPP '(0 0) vec)])
    (list (/ (list-ref vec 0) L) (/ (list-ref vec 1) L)))
  )
;(normalizeVector '(5 8))

(define (vectorAdd vec_one vec_two) ; Vector addition 
  (let(
       [x (+(list-ref vec_one 0)(list-ref vec_two 0))]
       [y (+(list-ref vec_one 1)(list-ref vec_two 1))])
    (list x y)
    ))

;;;(vectorAdd '(1 2) '(4 5))

(define (vectorSub vec_one vec_two) ; Vector substraction
  (let(
       [x (-(list-ref vec_one 0)(list-ref vec_two 0))]
       [y (-(list-ref vec_one 1)(list-ref vec_two 1))])
    (list x y)
    ))

;;;(vectorSub '(1 2) '(4 5))

(define (setVectorLength vector1 length1)
  (let ([nv (normalizeVector vector1)]
        [l (list length1 length1 length1)])
    (dotProduct nv l)))

;(normalizeVector '(1 1))
;(setVectorLength '(1 1) 2)
;(normalizeVector '(1 2))
;(setVectorLength '(1 2) 2)

;(define (bounce v n) ;v velosity vector, n normal vector
;  (let* ([Vfl (vectorAdd (dotProduct n (dotProduct (dotProduct v n) '(-2 -2))) v)] ;Frictionless
;         [Vf  (dotProduct Vfl '(0.9 0.9))]) ;With friction
;    Vf
;    ))
;
;;(bounce '(-3 0) (normalizeVector'(2 1)))



    