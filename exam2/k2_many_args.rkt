#lang racket

(define (double x) (* x 2))
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define xs '(1 2 3))
(define ys '(4 5 6 7))

; additional
(define (foldr1 f nv xs)
  (if (null? xs)
      nv
      (f (car xs) (foldr1 f nv (cdr xs)))
      )
  )

;Ex 1 compose
(define (compose . fn)
  (lambda (x)
    (if (null? fn)
        x
        ((car fn) ((apply compose (cdr fn)) x))
        )
    )
  )

(define f (compose double square inc))

;Ex 2 flip
(define (flip fn)
  (lambda x
    (apply fn (reverse x))
    )
  )

; additional
(define (reverse1 xs)
  (foldl cons '() xs)
  )

;Ex 3
(define (zip xs ys)
  (if (or (null? xs)
          (null? ys))
      '()
      (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys)))
      )
  )

(define (zip-with f xs ys)
  (if (or (null? xs)
          (null? ys))
      '()
      (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys)))
      )
  )

(define (any-empty? . ls)
  (or  (null? (car ls))
       (and  (not (null? (cdr ls)))
             (apply any-empty? (cdr ls)))          
       )
  )

(define (zip-with1 f . ls)
  (if (apply any-empty? ls)
      '()
      (cons (apply f (map car ls)) (apply zip-with1 f (map cdr ls)))
      )
  )

;Ex4

(define (juxt . fns)
  (lambda xs
    (if (null? fns)
        '()
        (cons (apply (car fns) xs) (apply (apply juxt (cdr fns)) xs))
        )
    )
  )

(define f1 (juxt inc dec square double)) ; (f x) = (list (inc x) (dec x) (square x) (double x))
;(f1 5) ; => (6 4 25 10)

(define g (juxt + *))
;(g 3 4 5) ; => (12 60)