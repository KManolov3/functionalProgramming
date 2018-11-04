#lang racket

(car (cons 1 (cons 2 (cons 3 '()))))

(define (my-length xs)
  (if (null? xs)
      0
      (+ 1 (my-length (cdr xs)))
      )
  )

(my-length (cons 1 (cons 2 (cons 3 '()))))

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))
      )
  )

(sum (cons 1 (cons 2 (cons 3 '()))))

;(define (accumulate xs null-value oper alpha-oper pred?)
 ; (if (pred? xs)
 ;     null-value
  ;    (oper (alpha (

(define (prod xs)
  (if (null? xs)
      1
      (* (car xs) (prod (cdr xs)))
      )
  )

(prod (cons 1 (cons 2 (cons 4 '()))))

(define (index n xs)
  (if (= n 1)
      (car xs)
      (index (- n 1) (cdr xs))
      )
  )

(index 2 (cons 1 (cons 2 (cons 3 '()))))

(define (fromTo a b)
  (if (> a b)
      '()
      (cons a (fromTo (+ a 1) b))
      )
  )

(fromTo 1 5)
(fromTo 5 3)

(define (sum-interval a b)
  (sum (fromTo a b))
  )

(sum-interval 3 5)

(define (fact a b)
  (prod (fromTo a b))
  )

(define (mein-append xs ys)
  (if (= (length xs) 1)
      (cons (car xs) ys)
      (cons (car xs) (mein-append (cdr xs) ys))
      )
  )


(mein-append (fromTo 1 3) (fromTo 4 6))

(define (take n xs)
  (if (= n 0)
      '()
      (cons (car xs) (take (- n 1) (cdr xs)))
      )
  )

(take 3 (fromTo 1 6))

(define (drop n xs)
  (if (= n 0)
      xs
      (drop (- n 1) (cdr xs))
      )
  )

(drop 2 (fromTo 1 5))

(define (sublist n m xs)
   (take (- m n) (drop n xs))
  )

(sublist 2 4 (fromTo 1 5))


(define (reverse xs)
  (if (= (length xs) 1)
      (car xs)
      (cons (reverse (cdr xs)) (reverse (take (- (length xs) 1) xs)))
      )
  )

(reverse (fromTo 1 5))

(define (fold f nv xs)
  (if (null? xs)
      nv
      (f (car xs) (fold f nv (cdr xs)))
      )
  )

(define (mein-append2 xs ys)
  (fold cons ys xs)
  )

(mein-append (fromTo 1 3) (fromTo 4 6))

(define (mein-map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (mein-map f (cdr xs)))
      )
  )

(mein-map (lambda (x) (* x x)) (fromTo 2 5))

(define (mein-map2 f xs)
  (fold (lambda (x y) (cons (f x) y)) '() xs)
  )

(mein-map2 (lambda (x) (* x x)) (fromTo 2 5))

(define (filter p xs)
  (if (null? xs)
      '()
      (if (p (car xs))
          (cons (car xs) (filter p (cdr xs)))
          (filter p (cdr xs))
          )
      )
  )

(filter (lambda (x) (if (= (remainder x 2) 0) #t #f)) (fromTo 1 5))

(define (filter-fold p xs)
  (fold (lambda (x y)
          (if (p x)
              (cons x y)
              y
              )
          )
        '()
        xs
        )
  )

(filter-fold (lambda (x) (if (= (remainder x 2) 0) #t #f)) (fromTo 1 5))


      