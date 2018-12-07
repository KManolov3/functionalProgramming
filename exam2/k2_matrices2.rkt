#lang racket
(require rackunit rackunit/text-ui)

(define m '((1 2 3) (4 5 6) (7 8 9)))

;Ex5

(define (transpose m)
  (if (null? (car m))
      '()
      (cons (map car m) (transpose (map cdr m)))
      )
  )

(define transpose-tests
  (test-suite
   "Tests for transpose"

   (check-equal? (transpose '((1))) '((1)))
   (check-equal? (transpose '((1) (2))) '((1 2)))
   (check-equal? (transpose '((1 2 3))) '((1) (2) (3)))
   (check-equal? (transpose '((1 2 3) (4 5 6))) '((1 4) (2 5) (3 6)))
   (check-equal? (transpose '((1 2 3) (4 5 6) (7 8 9)))
                 '((1 4 7) (2 5 8) (3 6 9)))))

;(run-tests transpose-tests)

;Ex6

(define (for-all-columns? p m)
  (and (p (map car m))
       (or (null? (cdar m))
           (for-all-columns? p (map cdr m))
           )
       )
  )

(define (any? p l)
  (and (not (null? l))
       (or (p (car l))
           (any? p (cdr l)))))

(define (odd-exists? l)
  (any? odd? l))

(define for-all-columns?-tests
  (test-suite
   "Tests for for-all-columns?"

   (check-true (for-all-columns? odd-exists? '((1))))
   (check-true (for-all-columns? odd-exists? '((1) (2))))
   (check-false (for-all-columns? odd-exists? '((1 2 3))))
   (check-true (for-all-columns? odd-exists? '((1 2 3) (4 5 6))))
   (check-true (for-all-columns? odd-exists? '((1 2 3) (4 5 6) (7 8 9))))
   (check-false (for-all-columns? odd-exists? '((1 2 3) (4 42 6) (7 8 9))))))

;(run-tests for-all-columns?-tests)

;Ex7

(define (prime? n)
  (define (prime-helper i)
    (if (= i n)
        #t
        (and (> (remainder n i) 0)
             (prime-helper (+ i 1)))
        )
    )
  (and (> n 1)
       (prime-helper 2)
       )
  )

(define (any?1 p xs)
  (if (null? xs)
      #f
      (or (p (car xs))
          (any?1 p (cdr xs)))
      )
  )

(define (prime-in-each-column? m)
  (if (null? (car m))
      #t
      (and (any?1 prime? (map car m))
           (prime-in-each-column? (map cdr m)))
      )
  )

(define prime-in-each-column?-tests
  (test-suite
   "Tests for prime-in-each-column?"

   (check-false (prime-in-each-column? '((1))))
   (check-true (prime-in-each-column? '((1) (2))))
   (check-false (prime-in-each-column? '((1 2 3))))
   (check-true (prime-in-each-column? '((1 2 3) (2 3 4))))
   (check-true (prime-in-each-column? '((17 2 16) (4 5 3))))
   (check-true (prime-in-each-column? '((1 2 3) (4 5 6) (7 8 9))))
   (check-false (prime-in-each-column? '((1 2 3) (4 5 6) (42 8 9))))))

;(run-tests prime-in-each-column?-tests)

;Ex8
(define num-rows length)
(define (num-cols m) (length (car m)))

(define (zip-with f xs ys)
  (if (or (null? xs)
          (null? ys))
      '()
      (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys)))
      )
  )

(define (calc-elem a b row-num col-num)
  (apply + (zip-with *
                     (list-ref a (- row-num 1))
                     (map (lambda (xs)
                              (list-ref xs (- col-num 1)))
                            b)
                     )
         )
  )

(define (calc-row a b row-num)
  (define (calc-row-helper col-num)
    (if (< (num-cols b) col-num)
        '()
        (cons (calc-elem a b row-num col-num)
              (calc-row-helper (+ col-num 1))
              )
        )
    )
  (calc-row-helper 1)
  )

(define (multiply a b)
  (define (multiply-helper row-num)
    (if (< (num-rows a) row-num)
        '()
        (cons (calc-row a b row-num)
              (multiply-helper (+ row-num 1))
              )
        )
    )
  (multiply-helper 1)
  )

(define multiply-tests
  (test-suite
   "Tests for multiply"

   (check-equal? (multiply '((2)) '((21))) '((42)))
   (check-equal? (multiply '((1) (2)) '((21))) '((21) (42)))
   (check-equal? (multiply '((0 21)) '((1) (2))) '((42)))
   (check-equal? (multiply '((1 2 3) (3 2 1) (1 2 3))
                           '((4 5 6) (6 5 4) (4 6 5)))
                 '((28 33 29) (28 31 31) (28 33 29)))))

;(run-tests multiply-tests)

;Ex9

(define m1 '((1 4 3) (4 5 6) (7 4 9)))

(define (all? p xs)
  (if (null? xs)
      #t
      (and (p (car xs))
           (all? p (cdr xs))
           )
      )
  )

(define (col-in-row? xs)
  (lambda (elem)
    (member elem xs))
  )
  

(define (is-col-in-row? col m)
  (any? (lambda (xs)
          (all? (col-in-row? xs) col)
          )
        m)
  )
        

(define (count-columns matrix)
  (define (count-columns-helper col-num)
    (if (> col-num (num-cols matrix))
        0
        (if (is-col-in-row? (map (lambda (xs)
                                   (list-ref xs (- col-num 1)))
                                 matrix)
                            matrix)
            (+ 1 (count-columns-helper (+ col-num 1)))
            (count-columns-helper (+ col-num 1))
            )
        )
    )
  (count-columns-helper 1)
  )

(define count-columns-tests
  (test-suite
   "Tests for count-columns"

   (check = (count-columns '((1))) 1)
   (check = (count-columns '((1 2))) 2)
   (check = (count-columns '((1) (2))) 0)
   (check = (count-columns '((1 3 5 7) (2 5 3 4))) 2)
   (check = (count-columns '((1 3 5 7) (2 5 3 7))) 3)
   (check = (count-columns '((1 4 3) (4 5 6) (7 4 9))) 1)
   (check = (count-columns '((1 4 3) (4 5 6) (7 3 9))) 0)))

;(run-tests count-columns-tests)