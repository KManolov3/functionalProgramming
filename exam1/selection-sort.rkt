#lang racket


(define (selection-sort xs)
  (define (remove-elem xs elem times)
    (if (null? xs)
        '()
        (if (and (= (car xs) elem)
                 (> times 0)
                 )
            (remove-elem (cdr xs) elem (- times 1))
            (cons (car xs) (remove-elem (cdr xs) elem times))
            )
        )
    )
  (if (null? xs)
      '()
      (let ((elem (foldr (lambda (x nv)
                           (if
                            (< x nv)
                            x
                            nv))
                         (car xs)
                         (cdr xs)))
            )
        (cons elem (selection-sort (remove-elem xs elem 1)))
        )
      )
  )

(selection-sort '(2 5 3 1 4))