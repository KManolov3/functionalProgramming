#lang racket

(define (apply twice f x)
  (f (f x))
  )

(define (succ n) (+ n 1))

(define (iterate f n x)
  (if (= n 0)
      x
      (iterate f (- n 1) (f x))
      )
   )

(define (iterate-two-args f n a b)
  (if (= n 1)
      a
      (iterate-two-args f (- n 1) (f a b) b)
      )
   )

(iterate succ 12 0)

;addition with iterate + succ
(define (my-plus a b)
  (iterate succ a b)
  )

(my-plus 10 15)



;(define (my-mult a b)
;  (iterate-two-args my-plus b a a)
;  )

;(my-mult 3 4)


(define (oper-interval mapper base oper a b)
  (if (> a b)
      base
      (oper (mapper a) (oper-interval mapper base oper (succ a) b))
      )
  )

(define (square a)
  (my-mult a a)
  )

(define (sum-after-square a b)
  (oper-interval (lambda (x) (* x x)) 0 + a b)
  )

(sum-after-square 2 5)

(define (my-mult x y)
  (if (= x 1)
      y
      (+ y (my-mult (- x 1) y)
         )
      )
  )

(define (my-expt x y)
  (if (= x 1)
      y
      (* y (my-expt (- x 1) y)
         )
      )
  )


(define (next-oper oper x y)
  (if (= x 1)
      y
      (oper y (next-oper oper (- x 1) y))
      )
  )

(next-oper my-mult 2 3)

(define (next-oper-lambda oper)
  (lambda (x y) (if (= x 1)
                     y
                     (oper y ((next-oper-lambda oper) (- x 1) y))
                     )        
    )
  )
(define (my-expt1) (next-oper-lambda my-mult))
((my-expt1) 10 2)
;((next-oper-lambda my-mult) 2 3)

;(define my-expt (next-oper my-mult))
;(define my-expt x y) (next-oper my-mult x y))

;f(g(x))
(define (compose f g)
  (lambda (x y) (f (g x) (g y)))
  )

(define (func1 x)
  (* x x)
  )
(define (func2 res1 res2)
  (+ res1 res2)
  )

((compose func2 func1) 2 3)


