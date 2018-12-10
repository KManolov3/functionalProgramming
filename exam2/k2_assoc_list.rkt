#lang racket

(define (make-alist fn keys)
  (map (lambda (key) (cons key (fn key))) keys)
  )

(define (keys alist)
  (map car alist)
  )

(define (values alist)
  (map cdr alist)
  )

(define (exists? p alist)
  (and (not (null? alist))
       (or (p (car alist))
           (exists? p (cdr alist)))
       )
  )

(define (assoc key alist)
  (exists? (lambda (key-value)
             (and (equal? key (car key-value))
                                    key-value)
             alist)
           )
  )

;;assv & assq - with eqv and eq

(define (del-assoc key alist)
  (filter (lambda (key-value) (not
                       (equal? key (car key-value))))
          alist)
  )

(define (add-assoc key value alist)
  (cons (cons key value) (del-assoc key alist))
  )
       