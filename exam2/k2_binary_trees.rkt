#lang racket
(require rackunit rackunit/text-ui)

;Ex1

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t))
           )
      )
  )

(define (make-tree root left right)
  (list root left right)
  )

(define (root tree)
  (car tree)
  )

(define (left tree)
  (cadr tree)
  )

(define (right tree)
  (caddr tree)
  )

(define (empty? tree)
  (null? tree)
  )

(define (leaf? tree)
  (and (empty? (left tree))
       (empty? (right tree))
       )
  )

;Ex2

;k-l-d
(define (pre-order tree)
  (if (null? tree)
      '()
      (cons (car tree) (append (pre-order (cadr tree)) (pre-order (caddr tree))))
      )
  )

;l-k-d
(define (in-order tree)
  (if (null? tree)
      '()
      (append (in-order (cadr tree)) (list (car tree)) (in-order (caddr tree)))
      )
  )

;l-d-k

(define (post-order tree)
  (if (null? tree)
      '()
      (append (post-order (cadr tree)) (post-order (caddr tree)) (list (car tree)))
      )
  )

(define traversals-tests
  (test-suite
   "Tests for traversals"

   (check-equal? (pre-order '(1 (2 (4 () ()) (5 () ())) (3 () ())))
                 '(1 2 4 5 3))
   (check-equal? (in-order '(1 (2 (4 () ()) (5 () ())) (3 () ()))) '(4 2 5 1 3))
   (check-equal? (post-order '(1 (2 (4 () ()) (5 () ())) (3 () ())))
                 '(4 5 2 3 1))))

;(run-tests traversals-tests)

;Ex3

(define tree '(1 (2 (4 () ()) (5 () ())) (3 () ())))

(define (level n tree)
  (if (null? tree)
      '()
      (if (= n 0)
          (list (car tree))
          (append (level (- n 1) (cadr tree)) (level (- n 1) (caddr tree)))
          )
      )
  )

(define level-tests
  (test-suite
   "Tests for level"

   (check-equal? (level 0 '(1 (2 (4 () ()) (5 () ())) (3 () ()))) '(1))
   (check-equal? (level 1 '(1 (2 (4 () ()) (5 () ())) (3 () ()))) '(2 3))
   (check-equal? (level 2 '(1 (2 (4 () ()) (5 () ())) (3 () ()))) '(4 5))))

;(run-tests level-tests)

;Ex4

(define (count-leaves1 tree)
  (cond
     (empty? tree) (0)
     (leaf? tree) (1)
     (else (+ (count-leaves (left tree)) (count-leaves (right tree))))
      )
  )

(define (count-leaves tree)
  (if (empty? tree)
      0
      (if (leaf? tree)
          1
          (+ (count-leaves (left tree)) (count-leaves (right tree)))
          )
      )
  )

(define count-leaves-tests
  (test-suite
   "Tests for count-leaves"

   (check = (count-leaves '()) 0)
   (check = (count-leaves '(1 () ())) 1)
   (check = (count-leaves '(1 (2 (4 () ()) (5 () ())) (3 () ()))) 3)))

;(run-tests count-leaves-tests)

;Ex5

(define (map-tree fn tree)
  (if (empty? tree)
      '()
      (list (fn (root tree)) (map-tree fn (left tree)) (map-tree fn (right tree)))
      )
  )

(define (square x) (* x x))
(define (cube x) (* x x x))


(define map-tree-tests
  (test-suite
   "Tests for map-tree"

   (check-equal? (map-tree square '()) '())
   (check-equal? (map-tree square '(4 () ())) '(16 () ()))
   (check-equal? (map-tree square '(1 (2 (4 () ()) (5 () ())) (3 () ())))
                 '(1 (4 (16 () ()) (25 () ())) (9 () ())))
   (check-equal? (map-tree cube '(1 (2 (4 () ()) (5 () ())) (3 () ())))
                 '(1 (8 (64 () ()) (125 () ())) (27 () ())))))

;(run-tests map-tree-tests)