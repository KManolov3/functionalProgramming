#lang racket

(define (sum-matrix matrix)
   (if (null? matrix)
       0
       (if (list? matrix)
           (+ (sum-matrix (car matrix)) (sum-matrix (cdr matrix)))
           matrix
           )
       )
  )

(sum-matrix '((1 2 3) (4 5 6)))

(define (diagonal-matrix matrix)
  (define (diagonal-matrix-helper matrix row)
    (if (null? matrix)
        '()
        (if (list? (car matrix))
            (cons (diagonal-matrix-helper (car matrix) row) (diagonal-matrix-helper (cdr matrix) (+ row 1)))
            (list-ref matrix row)
            )
        )
    )
  (diagonal-matrix-helper matrix 0)
  )

(diagonal-matrix '((1 2 3)
                 (4 5 6)
                 (7 8 9)))

(define (min-matrix matrix)
  (if (null? matrix)
      99999
      (if (list? (car matrix))
          (min (min-matrix (car matrix)) (min-matrix (cdr matrix)))
          (min (car matrix) (min-matrix (cdr matrix)))
          )
      )
  )

(min-matrix '((1 2 3)
                 (4 5 6)
                 (7 8 9)))

(define (accumulate op null-value start end term next)
  (if (> start end)
      null-value
      (op (term start) (accumulate op null-value (next start) end term next))
      )
  )

(define (average f g)
  (lambda (x) (/ (+ (f x) (g x)) 2))
  )

(define (calcprod f n)
  (accumulate * 1 1 n (lambda (i) ((average f (lambda (x) (expt i x))) i)) (lambda (x) (+ x 1)))
  )

(calcprod (lambda (x) (+ x 2)) 2)

(define (max-unique list)
  (define (is-unique elem list count)
    (if (null? list)
        (if (= count 1)
            #t
            #f
            )
        (if (= elem (car list))
            (is-unique elem (cdr list) (+ count 1))
            (is-unique elem (cdr list) count)
            )
        )
    )
  (define (find-max-unique list-original list-remainder)
    (if (null? list-remainder)
        0
        (if (is-unique (car list-remainder) list-original 0)
            (max (car list-remainder) (find-max-unique list-original (cdr list-remainder)))
            