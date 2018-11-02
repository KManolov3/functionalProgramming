#lang racket

(define (map function list)
  (if (null? list)
      '()
      (cons (function (car list)) (map function (cdr list)))
      )
  )

(map (lambda (x) (+ x 1)) '(1 2 3 4))

(define (filter predicate list)
  (if (null? list)
      '()
      (if (predicate (car list))
          (cons (car list) (filter predicate (cdr list)))
          (filter predicate (cdr list))
          )
      )
  )

(filter (lambda (x) (if (= (remainder x 2) 0) #t #f)) '(1 2 3 4))

(define (generate-interval start end)
  (if (> start end)
      '()
      (cons start (generate-interval (+ start 1) end))
      )
  )

(generate-interval 3 6)

(define (zip list1 list2)
  (if (or (null? list1) (null? list2))
      '()
      (cons (cons (car list1) (car list2)) (zip (cdr list1) (cdr list2)))
      )
  )

(zip '(1 2 3) '(4 5 5 6))

(define (member-deep? list element)
  (if (null? list)
      #f
      (if (equal? (car list) element)
          #t
          (or
           (if (list? (car list))
               (member-deep? (car list) element)
               #f)
           (member-deep? (cdr list) element)
           )
          )
      )
  )
(member-deep? '((1 2) 3 4) 5)


(define (my-flatten list) 
  (if (null? list)
      '()
      (if  (pair? (car list))
          (if (> (length (car list)) 1)
              (my-flatten (cons (car (car list)) (cons (cdr (car list)) (cdr list))))
              (my-flatten (cons (car (car list)) (cdr list)))
              )
          (cons (car list) (my-flatten (cdr list)))
          )
      )
  )

(my-flatten '((1 2) (1 2) 3 (1 2 (3 4))))


(define (fold f nv xs)
  (if (null? xs)
      nv
      (f (car xs) (fold f nv (cdr xs)))
      )
  )

(define (insertion-sort list)
  (define (insert-element element sorted-list)
    (if (null? sorted-list)
        (cons element '()) 
        (if (> element (car sorted-list))
            (cons (car sorted-list) (insert-element element (cdr sorted-list) ))
            (cons element sorted-list)
            )
        )
    )
  (fold insert-element '() list)
  )

(insertion-sort '(2 5 1 6 2 4))

(define (intersect list1 list2)
  (define (contains? elem list)
    (if (null? list)
        #f
        (if (= (car list) elem)
            #t
            (contains? elem (cdr list))
            )
        )
    )
  (if (null? list1)
      '()
      (if (contains? (car list1) list2)
          (cons (car list1) (intersect (cdr list1) list2))
          (intersect (cdr list1) list2)
          )
      )
  )

(intersect '(1 2 3) '(4 3 5 2))
            
(define (union list1 list2)
  (define (remove-repetitions list)
    (fold (lambda (elem list) (if (member elem list) list (cons elem list))) '() list)
    )
  (remove-repetitions (my-flatten (cons list1 list2)))
 )

(union '(1 2 4 6) '(2 5 7 8))

(define (my-map f list)
  (if (null? list)
      '()
      (cons (f (car list)) (my-map f (cdr list)))
      )
  )

(define (my-filter pred list)
  (if (null? list)
      '()
      (if (pred (car list))
          (cons (car list) (my-filter pred (cdr list)))
          (cdr list)
          )
      )
  )

(define (fold2 f nv list)
  (if (null? list)
      nv
      (f (car list) (fold2 f nv (cdr list)))
      )
  )

(define (my-map-fold f list)
  (fold2 f '() list)
  )

(define (compose function-list)
  (fold2 (lambda (func func-list) (lambda (x) (func x))) '() function-list))

((compose (list
    (lambda (x) (+ x 1))
    (lambda (x) (+ x 2))
    (lambda (x) (+ x 3)))) 1) 
  