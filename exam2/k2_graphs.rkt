#lang racket
(require rackunit rackunit/text-ui)

(define (make-alist fn keys)
  (map (lambda (key) (cons key (fn key))) keys)
  )

(define (assoc key alist)
  (exists? (lambda (key-value)
             (and (equal? key (car key-value))
                                    key-value)
             )
             alist)
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
       

(define empty-graph '())
(define (make-graph vs)
  (map (lambda (v) (cons v empty-graph)) vs)
  )

(define (vertices g)
  (map car g)
  )



(define (children v g)
  (if (null? g)
      '()
      (if (equal? (caar g) v)
          (cdar g)
          (children v (cdr g))
          )
      )
  )

(define (exists? p l)
  (and (not (null? l))
       (or (p (car l))
           (exists? p (cdr l)))
       )
  )

(define (children1 v g)
  (exists? (lambda (vertex) (and (equal? v (car vertex))
                                 (cdr vertex))
             )
           g)
  )

(define (edge? u v g)
  (exists? (lambda (child) (equal? child v))
           (children u g)
           )
  )

(define (map-children v fn g)
  (map fn (children g))
  )

(define (search-child v p g)
  (exists? (lambda (child) (and (p child)
                                child))
           (children v g)
           )
  )

(define (add-vertex v g)
  (if (assoc v g)
      g
      (add-assoc v '() g)
      )
  )

(define (remove-vertex v g)
  (del-assoc v g)
  )

(define (add-edge u v g)
  (if (member v (children u g))
      g
      (add-assoc u (cons v (children u g)) g)
      )
  )

(define (remove-edge u v g)
  (add-assoc u
             (filter (lambda (child)
                         (not (equal? child v)))
                       (children u g))
             g)
  )


;;Ex.2

(define (degree v g)
  (+  
   (length (filter (lambda (vertex)
                 (member v (children vertex g)))
               (vertices g)))
   (length (children v g))
   )
  )

;;Ex.3

(define (get-unique children)
  (if (null? children)
      '()
      (if (not (member (car children) (cdr children)))
          (cons (car children) (get-unique (cdr children)))
          (get-unique (cdr children))
          )
      )
  )

(define (edges g)
  (get-unique
   (flatten
    (map (lambda (u)
                     (children u g))
                   (vertices g))
              )
   )
  )

;;Ex.4

(define (every? p g)
  (or (null? g)
      (and (p (car g))
           (every? p (cdr g))
           )
      )
  )

(define (symmetric? g)
  (every?
   (lambda (v)
     (every?
      (lambda (child)
        (member v (children child g))
        )
      (children v g)
      )
    )
   (vertices g)
   )
  )

;;Еx.3b

(define graph '((1 2) (2 4) (4 5) (5) (6 2 4)))

(define (flatmap f l)
  (apply append
   (map f l)
   )
  )

(define (edges2 g)
  (flatmap
   (lambda (v)
     (map
      (lambda (child)
        (list v child)
        )
      (children v g)
      )
     )
   (vertices g)
   )
  )

;;Ex. 5

(define (invert g)
  (foldl 
   (lambda (edge nv)
     (add-edge (cadr edge) (car edge) nv)
     )
   (make-alist
    (lambda (u) '())
    (vertices g)
    )
   (edges2 g)
   )
  )

;;Ex.6

(define (path? u v g)
  (define (dfs path)
    (or (= (car path) v)
        (and (not (member (car path) (cdr path)))
             (exists?
              (lambda (child)
                (dfs (cons child path))
                )
              (children (car path) g)
              )
             )
        )
    )
  (dfs (list u))
  )

;;Ex. 7

(define (acyclic? g)
  (define (dfs path)
    (or (member (car path) (cdr path))
        (exists?
         (lambda (child)
           (dfs (cons child path))
           )
         (children (car path) g)
         )
        )
    )
  (every?
   (lambda (vertex)
     (not (dfs (list vertex)))
     )
   (vertices g)
   )
  )