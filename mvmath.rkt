;#lang racket

;(require math)

;Vector funktions

(define (vectorMake frompoint topoint) ;Make a vector
  (for/list ([f frompoint] [t topoint])
    (- f t))
  )

;(vectorMake '(2 2 2) '(4 4 4))

(define (vectorAdd vec_one vec_two)
  (for/list ([v1 vec_one][v2 vec_two])
    (+ v1 v2)))

;(vectorAdd '(1) '(4))
;(vectorAdd '(1 2) '(4 5))
;(vectorAdd '(1 2 3) '(4 5 6))

(define (vectorSub vec_one vec_two)
  (for/list ([v1 vec_one][v2 vec_two])
    (- v1 v2)))

;(vectorSub '(1) '(4))
;(vectorSub '(1 2) '(4 5))
;(vectorSub '(1 2 3) '(4 5 6))

(define (vectorInv vector) ;make inverse of vector
           (for/list ([v vector])
             (/ 1 v)))

;(inverse '(3))
;(inverse '(1 2))
;(inverse '(1 2 3))

(define (vectorMagn vector)
  (sqrt (foldl (lambda (a s) (+ s (expt a 2))) 0 vector))
  )

;(vectorMagn '(1 1))
;(vectorMagn '(1 1 1))

(define (vectorNorm vector);normalize vector, also "unit vector"
  (let ([vm (vectorMagn vector)])
    (for/list ([v vector]) (/ v vm))
    ))

;(normalizeVector '(5 8))
;(normalizeVector '(5 8 2))

(define (vectorScalarMult scalar vector) ;for setting lenght of normalized vector or just multiply
  (for/list ([v vector]) (* scalar v)))

;(vectorScalarMult 1 (normalizeVector '(2 5)))
;(vectorScalarMult 1 (normalizeVector '(2 5 1)))

(define (vectorScalarDiv vector scalar) ;Special case momentum to velocity!!
  (for/list ([v vector]) (/ v scalar)))

(define (vectorDotProduct vec_one vec_two)
  (foldl (lambda (a b s) (+ s (* a b))) 0 vec_one vec_two))

;(vectorDotProduct '(1 2) '(4 5))
;(vectorDotProduct '(1 2 3) '(4 5 6))

(define (vectorCrossProduct vec_one vec_two) ; Crossproduct of two 3D vectors
  (let(
       [x (- (* (list-ref vec_one 1)(list-ref vec_two 2))(* (list-ref vec_one 2)(list-ref vec_two 1)))]
       [y (- (* (list-ref vec_one 2)(list-ref vec_two 0))(* (list-ref vec_one 0)(list-ref vec_two 2)))]
       [z (- (* (list-ref vec_one 0)(list-ref vec_two 1))(* (list-ref vec_one 1)(list-ref vec_two 0)))])
    (list x y z)
))

;;;(vectorCrossProduct '(1 2 3) '(4 5 6))

(define (vectorVectorAngle vec_one vec_two)
  (acos (/ (vectorDotProduct vec_one vec_two) (* (vectorMagn vec_one) (vectorMagn vec_two)))))

;(*(vectorVectorAngle '(5 0) '(0 5)) (/ 180 pi))
;(*(vectorVectorAngle '(0 5 0) '(0 0 5)) (/ 180 pi))

;Matrix funktions

(define (transpose lists) ; collumns to rows!
  (apply map list lists))

;(transpose '((3 3)
;             (4 4)
;             (5 5)))

(define (dotProduct point mod_matrix); any size 1*3 matrix and upp... (multiplikation)
  (for/list ([m mod_matrix])
    (apply +
           (for/list ([p point] [n m])
             (* p n)))
    ))

;(dotProduct '(1 1 1) '((3 4 5)))
;(dotProduct '(1 1 1 1) '((2 2 2 1) (2 2 2 1) (2 2 2 1) (2 2 2 1)))

(define (dotProduct2 matrix1 matrix2);Any size matrices
  (let ([matrix2t (transpose matrix2)])
    (for/list ([m1 matrix1])
      (for/list ([m2 matrix2t])
        (apply +
           (for/list ([n1 m1][n2 m2])
             (* n1 n2)))
    )
    ))
  )

;(dotProduct2 '((1 1 1)
;              (2 2 2))
;            '((3 3)
;              (4 4)
;              (5 5)))

;Geodetic funktions sort of

(define (zInTriangle a b c point) ;;;3 point triangle and point to check for z
  (let*([v1 (vectorMake a b)]
        [v2 (vectorMake a c)]
        [n  (vectorCrossProduct v1 v2)]
        [k  (dotProduct n (list a))])
     (* (/ 1 (list-ref n 2))
        (- (list-ref k 0)
           (* (list-ref n 0)(list-ref point 0))
           (* (list-ref n 1)(list-ref point 1)))))
  )

;;;(zInTriangle '(0 0 1) '(5 0 1) '(0 5 1) '(2 2 0))

(define (pointInTriangle a b c point) ;;; Check if point is inside triangle
  (let* (
        [dn (+(*(-(list-ref b 1)(list-ref c 1))(-(list-ref a 0)(list-ref c 0)))
             (*(-(list-ref c 0)(list-ref b 0))(-(list-ref a 1)(list-ref c 1))))]
        [an (/(+(*(-(list-ref b 1)(list-ref c 1))(-(list-ref point 0)(list-ref c 0)))
             (*(-(list-ref c 0)(list-ref b 0))(-(list-ref point 1)(list-ref c 1)))) dn)]
        [bn (/(+(*(-(list-ref c 1)(list-ref a 1))(-(list-ref point 0)(list-ref c 0)))
             (*(-(list-ref a 0)(list-ref c 0))(-(list-ref point 1)(list-ref c 1)))) dn)]
        [cn (- 1 an bn)])
    (if (and (<= 0 an)(<= an 1)(<= 0 bn)(<= bn 1)(<= 0 cn)(<= cn 1))
        true false))
  )

;;;(pointInTriangle  '(1 1 0) '(5 0 0) '(0 5 0) '(2 2 0))
;;;(pointInTriangle  '(1 1 0) '(5 0 0) '(0 5 0) '(5 5 0))

(define (lengthPP p1 p2)
  (sqrt (foldl (lambda (a b s) (+ s (expt (- a b) 2))) 0 p1 p2))
  )

;(lengthPP '(1 1 1) '(0 0 0))
;(lengthPP '(0 0 0) '(1 1 1))

(define (axisRotation matrix rx ry rz);rotation around x/y/z axis
  (let* (
        [x (list (list 1 0 0 0)
                 (list 0 (cos rx) (* (sin rx) -1) 0)
                 (list 0 (sin rx) (cos rx) 0)
                 (list 0 0 0 1))]
        [y (list (list (cos ry) 0 (sin ry) 0)
                 (list 0 1 0 0)
                 (list (* (sin ry)-1) 0 (cos ry) 0)
                 (list 0 0 0 1))]
        [z (list (list (cos rz) (* (sin rz)-1) 0 0)
                 (list (sin rz) (cos rz) 0 0)
                 (list 0 0 1 0)
                 (list 0 0 0 1))]
        [rxy  (for/list ([rty y]) (dotProduct rty x))]
        [rxyz (for/list ([rtz z]) (dotProduct rtz rxy))])
        (for/list ([xyz matrix]) (dotProduct xyz rxyz))
    ))

;;;(axisRotation '((1 1 0 1)(3 3 0 1)(0 0 0 1)) 0 0 1)

(define (translationMatrix matrix dx dy dz) ; Moves object around
  (let ([tm (list (list 1 0 0 dx)
                  (list 0 1 0 dy)
                  (list 0 0 1 dz)
                  (list 0 0 0 1 ))])
    (for/list ([xyz matrix])(dotProduct xyz tm))))

;;;(translationMatrix '((2 2 2 1)(3 3 3 1)) 1 2 3)

(define (wDivide matrix) ; Makes the magic 3D effect come to life!
  (for/list ([point matrix])
    (list (/(list-ref point 0)(list-ref point 3))
          (/(list-ref point 1)(list-ref point 3))
          (/(list-ref point 2)(list-ref point 3))
          (/(list-ref point 3)(list-ref point 3)))
    ))

;;;(wDivide '((2 3 4 0.5)(2 3 4 0.2)))

(define (projectionMatrix d dx dy dz rx ry rz matrix) ;  d = angle of view, xyz position, xyz rotation, world. 
  (let* ([PM (list (list 1 0 0 0)
                   (list 0 1 0 0)
                   (list 0 0 1 0)
                   (list 0 0 (/ 1 d) 0))]
         [MatrixT (translationMatrix matrix (* -1 dx) (* -1 dy) (* -1 dz))]
         [MatrixTR (axisRotation MatrixT rx ry rz)]
         [cam (for/list ([xyz MatrixTR]) (dotProduct xyz PM))]
         [camc (for/list ([point cam] #:when (> (list-ref point 2) 0)) point )]
         )
    (if (> (length camc) 0) (wDivide camc) camc)))

;;;(projectionMatrix 50 1 2 3 0 0 0 '((1 2 1 1)(7 8 9 1)(4 5 6 1)))