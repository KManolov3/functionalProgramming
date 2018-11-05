#lang racket

(define (prime? n)
  (define (prime-helper? n divider)
    (if
     (= n divider)
     #t
     (if
      (= (remainder n divider) 0)
      #f
      (prime-helper? n (+ divider 1))
      )
     )
    
    )
  (prime-helper? n 2)
  )

(prime? 4)

(define (sum-interval a b)
  (define (sum-interval-helper a b curSum)
    (if
     (> a b)
     curSum
     (sum-interval-helper
      (+ a 1)
      b
      (+ curSum a)
      )
     )
    )
  (sum-interval-helper a b 0)
  )

(sum-interval 1 10)

(define (sum-interval-recursive a b)
  (if
   (= a b)
   a
   (+
    a
    (sum-interval-recursive (+ a 1) b)
    )
   )
  )

(sum-interval-recursive 1 10)

(define (fib n)
  (cond ((= 0 n) 1)
        ((= 1 n) 2)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fibonacci-iter n)
  (define (fib-help x1 x2 num)
    (if
     (= num n)
     (+ x1 x2)
     (fib-help x2 (+ x1 x2) (+ num 1))
     )
    )
  (fib-help 0 1 0)
  )

(time (fib 35))
(time (fibonacci-iter 35))

;
;(define (ackerman x y)
;  (define (ackerman-help x y acc)
 ;   (cond
  ;    (
   ;    (= 0 x) (+ y 1)
    ;           )
     ; (
      ; (= 0 y) (ackerman (- x 1) 1)
       ;        )
      ;(else
       ;(

(define (ackerman x y)
  (cond
    (
     (= 0 x) (+ y 1)
             )
    (
     (= 0 y) (ackerman (- x 1) 1)
             )
    (else
     (ackerman (- x 1) (ackerman x (- y 1)))
     )
    )
  )
(ackerman 4 2)