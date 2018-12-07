#lang racket

;;;;;; zad 1a

(define (accumulate op null-value start end term next)
  (if (> start end)
      null-value
      (op
          (term start)
          (accumulate op null-value (next start) end term next))
      )
)

(define (find-deg n)
  (if (= n 0)
      1
      (accumulate + 0 1 n (lambda (x) 1) (lambda (x) (* x 10)))
     )
  )

(define (narcissistic? n)
  (define (narcissistic-helper deg num accum)
    (if (= num 0)
        accum
        (narcissistic-helper deg (quotient num 10) (+ accum (expt (remainder num 10) deg) ))
        )
    )
  (= (narcissistic-helper (find-deg n) n 0) n)
  )

;(narcissistic? 153)

;;;;;;; zad 1b

(define (find-divisor-sum n)
  (accumulate + 0 1 (- n 1) (lambda (x) (if (= (remainder n x) 0) x 0)) (lambda (x) (+ x 1)))
  )

(define (friendly? n m)
  (and (= (find-divisor-sum n) m)
       (= n (find-divisor-sum m))
       )
  )

;(friendly? 220 284)

;;;;;; zad 2

(define (findMax rel a b)
  (if (= a (- b 1))
      (accumulate rel b a (- b 1) (lambda (x) x) (lambda (x) (+ x 1)))
      (max (accumulate rel b a (- b 1) (lambda (x) x) (lambda (x) (+ x 1))) (findMax rel (+ a 1) b) (findMax rel a (- b 1)))
      )
  )
;(findMax - 1 5)

;;;;;; zad 3a

(define (find-dif pair)
  (- (cdr pair) (car pair))
  )

(define (find-shortest-interval il)
  (define (find-shortest-interval-helper il length-of-shortest shortest-interval)
    (if (null? il)
        shortest-interval
        (if (< (find-dif (car il)) length-of-shortest)
            (find-shortest-interval-helper (cdr il) (find-dif (car il)) (car il))
            (find-shortest-interval-helper (cdr il) length-of-shortest shortest-interval)
            )
        )
    )
  (find-shortest-interval-helper (cdr il) (find-dif (car il)) (car il))
  )

(define (construct-superset-list smallest il)
  (if (null? il)
      '()
      (if (and (<= (caar il) (car smallest))
               (>= (cdar il) (cdr smallest))
               )
          (cons (car il) (construct-superset-list smallest (cdr il)))
          (construct-superset-list smallest (cdr il))
          )
      )
  )

(define test
  (list (cons 24 26) (cons 90 110) (cons 0 100) (cons 10 89) (cons 1 5) (cons -4 25)))

;(construct-superset-list (find-shortest-interval test) test)

;;;;;; zad 3b

(define (find-min xs-pair)
  (define (find-min-helper min-pair xs-pair)
    (if (null? xs-pair)
      min-pair
      (if (< (cdar xs-pair) (cdr min-pair))
          (find-min-helper (car xs-pair) (cdr xs-pair))
          (find-min-helper min-pair (cdr xs-pair))
          )
      )
    )
  (find-min-helper (car xs-pair) (cdr xs-pair))
  )

(define (remove-first-match pair xs-pair)
    (if (null? xs-pair)
        '()
        (if (= (cdr pair) (cdar xs-pair))
            (cdr xs-pair)
            (cons (car xs-pair) (remove-first-match pair (cdr xs-pair)))
            )
        )
  )

(define (selection-sort-pairs xs-pair)
  (let ((min-elem (find-min xs-pair)))
     (if (null? (cdr xs-pair))
         xs-pair
         (cons min-elem (selection-sort-pairs (remove-first-match min-elem xs-pair)))
         )
    )
  )

(define (shortest-interval-supersets il)
  (selection-sort-pairs (construct-superset-list (find-shortest-interval il) il))
  )

;(shortest-interval-supersets test)
      

  
                                         
         