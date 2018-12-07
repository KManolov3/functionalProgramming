#lang racket
(require rackunit rackunit/text-ui)


(define (middle-digit n)
  (define (get-len n len)
    (if (< n 1)
        len
        (get-len (quotient n 10) (+ len 1))
        )
    )
  (define (get-n-digit num digit)
    (if (< num (expt 10 digit))
        (remainder num 10)
        (get-n-digit (quotient num 10) digit)
        )
    )
  (let
      (
       (len (get-len n 0))
       )
    (if (= (remainder len 2) 0)
        -1
        (get-n-digit n (+ (quotient len 2) 1))
        )
    )
  )

;(middle-digit 4522432)


(define (meet-twice? f g a b)
  (define (meet-twice-help a b count)
    (cond ((= count 2) #t)
          ((> a b) #f)
          ((= (f a) (g a)) (meet-twice-help (+ a 1) b (+ count 1)))
          (else (meet-twice-help (+ a 1) b count)))
    )
  (meet-twice-help a b 0)
  )

;(meet-twice? (lambda(x)x) (lambda(x) (- x)) -3 1)
;(meet-twice? (lambda(x)x) sqrt 0 5)

(define (next-look-and-say xs)
  (define (next-look-and-say-helper xs cur-num num-times)
    (if (null? xs)
        (cons num-times (cons cur-num '()))
        (if 
          (= (car xs) cur-num)
          (next-look-and-say-helper (cdr xs) cur-num (+ num-times 1))
          (cons num-times (cons cur-num (next-look-and-say-helper (cdr xs) (car xs) 1)))
          )
        )
    )
  (if (null? xs)
      '()
      (next-look-and-say-helper (cdr xs) (car xs) 1)
      )
  )

;(next-look-and-say '(1 1 2 3 3))

(define (is-em? xs oper f)
  (define (contains? elem xs)
    (if (null? xs)
        #f
        (if (= (car xs) elem)
            #t
            (contains? elem (cdr xs))
            )
        )
    )
  (define (asociativity? elem xs)
    (if (null? xs)
        #t
        (if (= (oper (f elem) (f (car xs))) (f (oper elem (car xs))))
            (asociativity? elem (cdr xs))
            #f
            )
        )
    )
  (define (em-helper ys)
    (if (null? ys)
        #t
        (if (and (contains? (f (car ys)) xs) (asociativity? (car ys) xs))
            (em-helper (cdr ys))
            #f
            )
        )
    )
  (em-helper xs)
  )

;(is-em? '(0 1 4 6) + (lambda (x) (remainder x 3)))

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

;(calcprod (lambda (x) (+ x 2)) 3)

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
            (find-max-unique list-original (cdr list-remainder))
            )
        )
    )
  (if (null? list)
      0
      (max (find-max-unique (car list) (car list)) (max-unique (cdr list)))
      )
  )

;(max-unique '((1 2 3 2) (5 5 6) (0)))

(define (find-root f)
  (define (find-root-helper min max)
    (if (< (abs (f (/ (+ min max) 2))) (expt 10 -5))
        (/ (+ min max) 2)
        (if (< (* (f min) (f (/ (+ min max) 2))) 0)
            (find-root-helper min (/ (+ min max) 2))
            (find-root-helper (/ (+ min max) 2) max)
            )
        )
    )
  (define (find-min-max min max)
    (if (< (* (f min) (f max)) 0)
        (list min max)
        (find-min-max (* min 2) (* max 2))
        )
    )
  (let ((min-max (find-min-max -10 10))
        )
    (find-root-helper (list-ref min-max 0) (list-ref min-max 1))
    )
  )

(find-root (lambda (x) (- (* 3 x) 6)))

(define (longest-descending xs)
  (define (longest-descending-helper xs prev cur-max abs-max)
    (if (null? xs)
        abs-max
        (if (> prev (car xs))
            (longest-descending-helper (cdr xs) (car xs) (cons (car xs) cur-max) abs-max)
            (if (> (length cur-max) (length abs-max))
                (longest-descending-helper (cdr xs) (car xs) (cons (car xs) '()) cur-max)
                (longest-descending-helper (cdr xs) (car xs) (cons (car xs) '()) abs-max)
            )
        )
    )
    )
  (reverse (longest-descending-helper (cdr xs) (car xs) (cons (car xs) '()) '()))
  )

(longest-descending '(5 3 8 6 4 2 6 7 1))


(define endomorphism?-tests
  (test-suite
   "Tests for endomorphism?"

   (check-true (is-em? '() + (lambda (x) (remainder x 3))))
   (check-true (is-em? '(0 1 4 6) + (lambda (x) x)))
   (check-true (is-em? '(0 1 4 6) + (lambda (x) (remainder x 3))))
   (check-false (is-em? '(0 1 4 5 6) + (lambda (x) (remainder x 3))))
   (check-false (is-em? '(0 1 4 6) expt (lambda (x) (+ x 1))))))

(run-tests endomorphism?-tests)

(define meet-twice?-tests
  (test-suite
   "Tests for meet-twice?"

   (check-true (meet-twice? (lambda (x) x) (lambda (x) x) 0 5))
   (check-true (meet-twice? (lambda (x) x) sqrt 0 5))
   (check-false (meet-twice? (lambda (x) x) (lambda (x) x) 42 42))
   (check-false (meet-twice? (lambda (x) x) (lambda (x) (- x)) -3 1))))

(run-tests meet-twice?-tests)

(define middle-digit-tests
  (test-suite
   "Tests for middle-digit"

   (check = (middle-digit 0) 0)
   (check = (middle-digit 1) 1)
   (check = (middle-digit 42) -1)
   (check = (middle-digit 452) 5)
   (check = (middle-digit 4712) -1)
   (check = (middle-digit 47124) 1)
   (check = (middle-digit 1892364) 2)
   (check = (middle-digit 38912734) -1)))

(run-tests middle-digit-tests)

(require rackunit rackunit/text-ui)

(define next-look-and-say-tests
  (test-suite
   "Tests for next-look-and-say"

   (check-equal? (next-look-and-say '()) '())
   (check-equal? (next-look-and-say '(1)) '(1 1))
   (check-equal? (next-look-and-say '(1 1 2 3 3)) '(2 1 1 2 2 3))
   (check-equal? (next-look-and-say '(1 1 2 2 3 3 3 3)) '(2 1 2 2 4 3))))

(run-tests next-look-and-say-tests)









           
