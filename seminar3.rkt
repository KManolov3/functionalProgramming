#lang racket

(define (generic-sum a b term next)
  (if (> a b)
      0
      (+ (term a) (generic-sum (next a) b term next))
      )
  )

(define (succ a)
  (+ a 1)
  )

(define (id a)
  a
  )

(generic-sum 1 3 id succ)

(define (accumulate start end term next null-value op)
  (if (> start end)
      null-value
      (op (term start) (accumulate (next start) end term next null-value op))
      )
  )

(accumulate 1 3 id succ 1 *)

(define (apply-twice f arg)
  (define (apply-twice-helper iter arg)
    (if (< iter 2)
        (apply-twice-helper (+ iter 1) (f arg))
        arg
        )
    )
  (apply-twice-helper 0 arg)
  )

(apply-twice (lambda (x) (* x x)) 5)

(define (apply-n number f arg)
  (if (= number 1)
      (f arg)
      (apply-n (- number 1) f (f arg))
  )
  )

(apply-n 2 (lambda (x) (* x x)) 5)

(define (factorial number)
  (accumulate 1 number id (lambda (x) (+ x 1)) 1 *)
  )

(factorial 5)

(define (double-factorial number)
  (accumulate 1 number id (lambda (x) (+ x 2)) 1 *)
  )

(double-factorial 5)

(define (power base exp)
  (accumulate 1 exp (lambda (x) base) (lambda (x) (+ x 1)) 1 *)
  )

(power 2 6)

(define (filter-accumulate start end term next null-value op filter)
  (if (> start end)
      null-value
      (if (= (filter start) 1)
          (op (term start) (filter-accumulate (next start) end term next null-value op filter))
          (filter-accumulate (next start) end term next null-value op filter)
      )
  )
)

(define (count p? a b)
  (filter-accumulate a b (lambda (x) 1) (lambda (x) (+ x 1)) 0 + p?)
  )

(count (lambda (x) (if (= (remainder x 2) 1) 1 0) ) 2 8)

(define (sum-divisors number)
  (filter-accumulate 1 (/ number 2) (lambda (x) x) (lambda (x) (+ x 1)) 0 + (lambda (x) (if (= (remainder number x) 0) 1 0)))
  )

(sum-divisors 6)
