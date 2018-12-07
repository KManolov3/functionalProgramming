#lang racket
(require rackunit rackunit/text-ui)

(define m '((1 2 3) (4 5 6) (7 8 9)))

;Ex1 dimensions

(define (dimensions matrix)
  (cons (length matrix) (length (car matrix)))
  )

(define dimensions-tests
  (test-suite
   "Tests for dimensions"

   (check-equal? (dimensions '((1))) '(1 . 1))
   (check-equal? (dimensions '((1) (2))) '(2 . 1))
   (check-equal? (dimensions '((1 2 3) (4 5 6))) '(2 . 3))
   (check-equal? (dimensions '((1 2 3) (4 5 6) (7 8 9))) '(3 . 3))))

;(run-tests dimensions-tests)

;Ex2 reverse-columns

(define (reverse-columns matrix)
  (if (null? matrix)
      '()
      (cons (reverse (car matrix)) (reverse-columns (cdr matrix)))
      )
  )

;;Alt take

(define (reverse-columns1 matrix)
  (foldr (lambda (xs ls)
           (cons (reverse xs) ls)) '() matrix)
  )

;;Alt take2

(define (reverse-columns2 matrix)
  (map reverse matrix)
  )

(define reverse-columns-tests
  (test-suite
   "Tests for reverse-columns"

   (check-equal? (reverse-columns '((1))) '((1)))
   (check-equal? (reverse-columns '((1) (2))) '((1) (2)))
   (check-equal? (reverse-columns '((1 2 3))) '((3 2 1)))
   (check-equal? (reverse-columns '((1 2 3) (4 5 6))) '((3 2 1) (6 5 4)))
   (check-equal? (reverse-columns '((1 2 3) (4 5 6) (7 8 9)))
                 '((3 2 1) (6 5 4) (9 8 7)))))

;(run-tests reverse-columns-tests)

;Ex3 nth-column

(define (get-nth n xs)
  (if (null? xs)
      '()
      (if (= n 0)
          (car xs)
          (get-nth (- n 1) (cdr xs))
          )
      )
  )

(define (nth-column matrix n)
  (map (lambda (xs) (get-nth (- n 1) xs)) matrix)
  )

;;Alt take

(define (nth-column1 matrix n)
  (map (lambda (row)
         (list-ref row (- n 1))
         )
       matrix
       )
  )

(define nth-column-tests
  (test-suite
   "Tests for nth-column"

   (check-equal? (nth-column '((1)) 1) '(1))
   (check-equal? (nth-column '((1) (2)) 1) '(1 2))
   (check-equal? (nth-column '((1 2 3)) 1) '(1))
   (check-equal? (nth-column '((1 2 3)) 2) '(2))
   (check-equal? (nth-column '((1 2 3)) 3) '(3))
   (check-equal? (nth-column '((1 2 3) (4 5 6)) 1) '(1 4))
   (check-equal? (nth-column '((1 2 3) (4 5 6)) 2) '(2 5))
   (check-equal? (nth-column '((1 2 3) (4 5 6)) 3) '(3 6))
   (check-equal? (nth-column '((1 2 3) (4 5 6) (7 8 9)) 3) '(3 6 9))
   (check-equal? (nth-column '((1 2 3 4) (5 6 7 8)) 4) '(4 8))))

;(run-tests nth-column-tests)

;Ex4

(define (main-diagonal matrix)
  (define (main-diagonal-helper matrix n)
    (if (null? matrix)
        '()
        (cons (list-ref (car matrix) n) (main-diagonal-helper (cdr matrix) (+ n 1)))
        )
    )
  (main-diagonal-helper matrix 0)
 )

;Alt take

(define (enum-int from to)
  (if (> from to)
      '()
      (cons from
            (enum-int (+ from 1) to)
            )
      )
  )

(define (map-with-index f l)
  (map f l (enum-int 0 (- (length l) 1)))
  )

(define (main-diagonal1 matrix)
  (map-with-index list-ref matrix)
  )