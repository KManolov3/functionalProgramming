#lang racket

(define (accumulate op null-value start end term next)
  (if (> start end)
      null-value
      (op
          (term start)
          (accumulate op null-value (next start) end term next))
      )
)

(define (foldl f nv xs)
  (if (null? xs)
      nv
      (foldl f (f nv (car xs)) (cdr xs))
      )
  )

(define (foldr f nv xs)
  (if (null? xs)
      nv
      (f (car xs) (foldr f nv (cdr xs)))
      )
  )

; syntax forms:
 ; (cond
 ;   (pred1 expr1)
 ;   (pred2 expr2)
 ;   (else expr3))

 ;(let
 ;    ((variable1 value1)
 ;     (variable2 value2)
 ;     ;; ...
 ;     (variablen valuen))
 ;  body)

 ;let* - same as above

; functions for numbers:
 ;expt
 ;sqrt
 ;min
 ;max

; functions for lists:
 ;list?
 ;list
 ;length
 ;append
 ;reverse
 ;member?

; =/eq?/eqv?/equal?