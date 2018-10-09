#lang racket

(define
  (name gosho pesho)
  (* gosho pesho)
  )
(if (eq?(name 4 3) 12) 4 5)
(define (succ x) (+ x 1))
(define (pred x) (- x 1))

;(define (my-plus x y)
;  (if (> y 0) (my-plus (succ x) (pred y)) x))
; doesnt work for negatives

(define (my-plus1 x y)
  (if (= x 0) y
      (succ (my-plus1 (pred x) y))))

(define (my-mult x y)
  (if (= x 1) y
      (my-plus1 y (my-mult (pred x) y))
      )
  )

(define (my-pow x y)
  (if (= y 1) x
      (my-mult x (my-pow x (pred y)))
      )
  )

(define (fast-pow x y)
  (if (= y 1) x
      (* (fast-pow (* x x) (quotient y 2))
               (if (= (remainder y 2) 1) x 1) )
      )
  )

(define (factoriel x)
  (if (= x 1) x
      (my-mult (factoriel (pred x)) x)
      )
  )

(define (fibonacci x)
  (if (= x 1) 1
      (if (= x 0) 1
          (+ (fibonacci(pred x)) (fibonacci(pred (pred x))))
          )
      )
  )

(fibonacci 20)



                                           

                                         

