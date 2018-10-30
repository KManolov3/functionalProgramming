#lang racket


(define (minus-from n xs)
  (if (null? xs)
      n
      (minus-from (- n (car xs)) (cdr xs))
     )
  )

(minus-from 10 '(1 2 3))

(define (divide-by n xs)
  (if (null? xs)
      n
      (divide-by (/ n (car xs)) (cdr xs))
      )
  )

(divide-by 10 '(1 2 3))

(define (my-reverse xs)
  (if (null? xs)
      '()
      (append (my-reverse (cdr xs)) (list (car xs)))
      )
  )

(my-reverse '(1 2 3))

(define (my-reverse2 xs)
  (define (my-reverse-help xs accum)
    (if (null? xs)
        accum
        (my-reverse-help (cdr xs) (cons (car xs) accum))
        )
    )
  (my-reverse-help xs '())
  )

(my-reverse2 '(1 2 3))

(define (foldl f nv xs)
  (if (null? xs)
      nv
      (foldl f (f nv (car xs)) (cdr xs))
      )
  )

(define (my-reverse3 xs)
  (foldl (lambda (nv x) (cons x nv)) '() xs)
  )

(my-reverse3 '(1 2 3))

(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))
      )
  )

(zip '(1 2 3) '(4 5 6))

(list '(1 2 3) '(4 5 6))

(define (zipwith f xs ys)
  (if (or (null? xs) (null? ys))
      0
      (f (f (car xs) (car ys)) (zipwith f (cdr xs) (cdr ys)))
      )
  )

(zipwith + '(1 2 3) '(4 5 6))

;(define (zip2 xs ys)
 ; (foldl ((lambda (nv xsys) (cons (cons (car xsys (car xsys)) (car xsys (cdr xsys))) nv)), '(), (list xs ys))
(car (car '((123))))

(define (concat xss)
  (if (null? xss)
      '()
      (if (list? (car xss))
          (append (concat (car xss)) (concat (cdr xss)))
          (cons (car xss) (concat (cdr xss)))
          )
      )
  )
(concat '((1 (2 3)) (1 2 4) (4 5 6)))

(define (cartesian xss yss)
  ;(define (make-pair elem yss accum)
    ;(if (null? yss)
     ;   accum
    ;    (make-pair elem (cdr yss) (cons (cons elem (car yss)) accum)) 
   ;     )
  ;  )
  (if (null? xss)
      '()
      ;(append (make-pair (car xss) yss '()) (cartesian (cdr xss) yss))
      (append (foldl (lambda (nv ys) (cons (cons (car xss) ys) nv)) '() yss) (cartesian (cdr xss) yss)) 
      )
  )

(cartesian '(1 2 3) '(3 4 5))

(define (foldr2 f nv xs ys)
  (if (null? xs)
      nv
      (f (car xs) ys (foldr2 f nv (cdr xs) ys))
      )
  )
;(foldr2 (lambda (xs ys nv) (append (foldl (lambda (nv ys) (cons (cons xs ys) nv)) '() ys) nv)) '() xss yss)


(define (cartesian2 xss yss)
  (foldr2 (lambda (xs ys nv) (append (foldl (lambda (nv ys2) (cons (cons xs ys2) nv)) '() ys) nv)) '() xss yss)
  )

(cartesian2 '(1 2 3) '(3 4 5))

      
      