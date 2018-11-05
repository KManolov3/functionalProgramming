#lang racket

(define
  (even? number)
  (if (=
       (remainder number 2)
       1
       )
      #f
      #t
      )
  )

(define
  (pred a b c)
  (if 
   (or
       (< (+ a b) c)
       (< (+ b c) a)
       (< (+ a c) b)
       (<= a 0)
       (<= b 0)
       (<= c 0)
       )
       #f
       #t
       )
  )

(define
  (dtb a)
  (if
   (<= a 1)
   a
   (+
    (remainder a 2)
    (* 10
       (dtb (quotient a 2))
       )
    )
   )
  )

(define
  (factorial a)
  (if
   (= a 1)
   1
   (*
    a
    (factorial (- a 1))
    )
   )
  )

(define
  (sum a b)
  (if
   (<= a b)
   (+
    a
    (sum
     (+ a 1)
     b
     )
    )
   0
   )
  )

(define
  (power a b)
  (if
   (= b 0)
   1
   (if
    (= b 1)
    a
    (*
     (power
      (* a a)
      (quotient b 2)
      )
     (power
      a
      (remainder b 2)
      )
     )
    )
   )
  )

(define
  (gcd a b)
  (if
   (= b 0)
   a
   (gcd
    b
    (remainder a b)
    )
   )
  )

(define
  (reverse number)
  (define
     (reverse-iter number curRev)
     (if
      (< number 10)
      (+
        number
        (* 10 curRev)
        )
      (reverse-iter
       (quotient number 10)
       (+
        (remainder number 10)
        (* 10 curRev)
        )
       )
      )
    )
   (reverse-iter number 0)
  
   )


                   
  
