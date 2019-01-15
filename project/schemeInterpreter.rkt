#lang racket

(define (search p l)
  (and (not (null? l))
       (or (p (car l))
           (search p (cdr l)))))

(define (make-alist fn keys)
  (map (lambda (key) (cons key (fn key)))
       keys))

(define (assoc key alist)
  (search (lambda (key-value-pair)
            (and (equal? (car key-value-pair) key)
                 key-value-pair))
          alist))

(define (del-assoc key alist)
  (filter (lambda (key-value-pair)
            (not (equal? (car key-value-pair) key)))
          alist))

(define (add-assoc key value alist)
  (cons (cons key value)
        (del-assoc key alist)))

(define (interpret code)
  (if (> (length code) 1)
      (interpret (replace (cdar code) (cdr code)))
      (evaluate (car code))
      )
  )


(define (replace definition restOfCode)
  (if (= (length restOfCode) 0)
      '()
      (if (function-definition? (car definition))
          (cons (replace-in-clause (caar definition) (construct-lambda-expression (cdar definition) (cadr definition)) (car restOfCode)) (replace definition (cdr restOfCode)))
          (cons (replace-in-clause (car definition) (cadr definition) (car restOfCode)) (replace definition (cdr restOfCode)))
          )
      )
  )

(define (function-definition? definitionSignature)
  (list? definitionSignature)
  )

(define (construct-lambda-expression params body)
  (list 'lambda params body)
  )

(define (replace-in-clause definitionName definitionBody clause)
  (if (null? clause)
      '()
      (if (and
           (non-empty-list? clause)
           (non-empty-list? (cdr clause))
           (definition-with-same-name-parameter? definitionName (car clause) (cadr clause)))
          clause
          (if (atom? clause)
              (replace-if-match definitionName definitionBody clause)
              (cons (replace-in-clause definitionName definitionBody (car clause)) (replace-in-clause definitionName definitionBody (cdr clause)))
              )
          )
      )
  )

(define (definition-with-same-name-parameter? definitionName clauseType clauseParams)
  (if (or
       (equal? clauseType 'define)
       (equal? clauseType 'lambda)
       )
      (if (list? clauseParams)
          (member definitionName clauseParams)
          #f
          )
      #f
      )
  )

(define (atom? clause)
  (not (list? clause))
  )

(define (non-empty-list? clause)
  (and (list? clause)
       (not (null? clause))
       )
  )

(define (replace-if-match definitionName definitionBody atom)
  (if (equal? atom definitionName)
      definitionBody
      atom
      )
  )

(define (evaluate clause)
  ;(and
   ;(println clause)
   (if (or
        (atom? clause)
        (null? clause)
        )
       clause
       (let ((clause-op (evaluate (car clause))))
         (cond ((equal? '+ clause-op) (apply + (map evaluate (cdr clause))))
               ((equal? '- clause-op) (apply - (map evaluate (cdr clause))))
               ((equal? '* clause-op) (apply * (map evaluate (cdr clause))))
               ((equal? '/ clause-op) (apply / (map evaluate (cdr clause))))
               ((equal? 'quotient clause-op) (apply quotient (map evaluate (cdr clause))))
               ((equal? 'remainder clause-op) (apply remainder (map evaluate (cdr clause))))
               ((equal? 'max clause-op) (apply max (map evaluate (cdr clause))))
               ((equal? 'min clause-op) (apply min (map evaluate (cdr clause))))
               ((equal? '> clause-op) (apply > (map evaluate (cdr clause))))
               ((equal? '>= clause-op) (apply >= (map evaluate (cdr clause))))
               ((equal? '< clause-op) (apply < (map evaluate (cdr clause))))
               ((equal? '<= clause-op) (apply <= (map evaluate (cdr clause))))
               ((equal? 'null? clause-op) (apply null-quoted? (map evaluate (cdr clause))))
               ((equal? 'list? clause-op) (apply list? (map evaluate (cdr clause))))
               ((equal? 'equal? clause-op) (apply equal? (map evaluate (cdr clause))))
               ((equal? '= clause-op) (apply = (map evaluate (cdr clause))))
               ((equal? 'list clause-op) (apply list (map evaluate (cdr clause))))
               ((equal? 'cons clause-op) (apply cons (map evaluate (cdr clause))))
               ((equal? 'car clause-op) (apply car (map evaluate (cdr clause))))
               ((equal? 'cdr clause-op) (apply cdr (map evaluate (cdr clause))))
               ((equal? 'if clause-op) (handle-if (cdr clause)))
               ((lambda? clause-op) (handle-lambda (cons clause-op (cdr clause))))
               ((equal? 'cond clause-op) (handle-cond (cdr clause)))
               (else clause))
         )
       )
  ;)
  )

(define (null-quoted? list)
  (or (null? list)
      (equal? list ''())
      )
  )

(define (handle-if args)
  (if (evaluate (car args))
      (evaluate (cadr args))
      (evaluate (caddr args))
      )
  )

(define (lambda? clause-op)
  (or
   (equal? 'lambda clause-op)
   (and (list? clause-op)
        (equal? 'lambda (car clause-op))
        )
   )
  )

(define (handle-lambda clause)
  ;(and
   ;(println clause)
  (if (list? (car clause))
      (calc-lambda (list-ref (car clause) 1) (list-ref (car clause) 2) (cdr clause))    
      clause)
   ;)
  )

(define (calc-lambda params body args)
  ;(and
   ;(println args)
   ;(println params)
   (if (null? params)
       (evaluate body)
       (calc-lambda (cdr params) (replace-in-clause (car params) (car args) body) (cdr args))
       )
   ;)
  )

(define (handle-cond cases)
  (if (= (length cases) 1)
      (evaluate (cadar cases))
      (if (evaluate (caar cases))
          (evaluate (cadar cases))
          (handle-cond (cdr cases))
          )
      )
  )

(define code1 '[(define (f x) (if (< x 3) (f (+ x 1)) (+ x 2)))
                (f 1)])


(interpret code1)



