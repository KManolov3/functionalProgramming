#lang racket

((lambda args (+ (car args) 5)) 2 3 4)

(define (double x) (* 2 x))
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (composeList f g)
  (lambda (x) (f (g x)))
  )

(define (compose . args)
  (foldr composeList (lambda (x) x) args)
  )

((compose double square inc) 3)


(define (flip fn)
  (lambda args
    (apply fn
           (reverse args)
           )
    )
  )

(define (list^ . args)
  (apply (flip list) args)
  )

(list^ 1 2 3)

(define (zip x y)
  (if (or (null? x)
          (null? y)
          )
      '()
      (cons (list (car x) (car y)) (zip (cdr x) (cdr y)))
      )
  )

(zip '(1 2 3) '(4 5 6))

(define (zip-with fn x y)
  (if (or (null? x)
          (null? y)
          )
      '()
      (cons (fn (car x) (car y)) (zip-with fn (cdr x) (cdr y)))
      )
  )

(zip-with + '(1 2 3) '(4 5 6))

(define (null-ls? . ls)
  (if (null? ls)
      #f
      (or (null? (car ls)) (apply null-ls? (cdr ls)))
      )
  )


(define (zip-with-ls fn . ls)
  (if (or (not (null? (filter null? ls)))
          (null? ls)
          )       
      '()
      (cons (apply fn (map car ls)) (apply zip-with-ls fn (map cdr ls)))
      )
  )
(zip-with-ls + '(1 2 3) '(4 5 6) '(7 8 9))



(define (juxt . fns)
  (if (null? fns)
      (λ args '())
      (λ args
        (cons (apply (car fns) args)
              (apply (apply juxt (cdr fns)) args)
              )
        )         
      )
  )

((juxt * +) 3 4 5)

(define (count-elems ls)
  (if (null? ls)
      0
      (+ 1 (count-elems (cdr ls)))
      )
  )

(define (dimensions matrix)
  (cons (count-elems matrix) (count-elems (car matrix)))
  )
(dimensions '((1 2 3) (4 5 6)))

(define (reverse-columns matrix)
  (if (null? matrix)
      '()
      (cons (foldl cons '() (car matrix)) (reverse-columns (cdr matrix)))
      )
  )

(reverse-columns '((1 2 3) (4 5 6)))