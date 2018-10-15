#lang racket

(define (sum-divisors number)
  (define (sum-divisors-help i sum)
    (if (< i number)
        (if (= (remainder number i) 0)
            (sum-divisors-help (+ i 1) (+ i sum))
            (sum-divisors-help (+ i 1) sum))
        sum
        )
    )
  (sum-divisors-help 1 0)
  )

(sum-divisors 6)

(define (increasing-digits? number)
  (if (< number 10)
      #t
      (if (< (remainder number 10) (quotient (remainder number 100) 10))
          #f
          (increasing-digits? (quotient number 10))
          )
      )
  )

(increasing-digits? 12134)

(define (binary-to-decimal number)
  (define (binary-to-decimal-help number sum power)
    (if (<= number 0)
        sum
        (binary-to-decimal-help
         (quotient number 10)
         (+ sum (*
                 (remainder number 10)
                 power)
            )
         (* power 2)
         )
        )
    )
  (binary-to-decimal-help number 0 1)
  )

(binary-to-decimal 1001)

(define (contains? a b)
  (define (contains-help num-holder b-left)
    (if (= b-left 0)
        #t
        (if (= (remainder num-holder 10) (remainder b-left 10))
            (contains-help (quotient num-holder 10) (quotient b-left 10))
            #f
            )
        )
    )
  (if (< a b)
      #f
      (if (contains-help a b)
          #t
          (contains? (quotient a 10) b)
          )
      )
  )

(contains? 123 23)

(define (palindrome? number)
  (define (find-pow number n)
    (if (= number 0)
        n
        (find-pow (quotient number 10) (+ n 1))
        )
    )
  (define (palindrome?-help number n)
    (if (and (< number 10) (<= n 1))
        #t
        (if (=
             (quotient number (expt 10 (- n 1)))
             (remainder number 10)
             )
            (palindrome?-help
             (quotient (remainder number (expt 10 (- n 1))) 10)
             (- n 2)
             )
            #f
            )
        )
    )
  (palindrome?-help number (find-pow number 0))
  )

(palindrome? 1331)

(define (contains-palindrome? number)
  (if (> number 10)
      (if (or
           (or (palindrome? (remainder number 100)) (= (remainder number 100) 0))
           (or (palindrome? (remainder number 1000)) (= (remainder number 1000) 0))
           )
          #t
          (contains-palindrome? (quotient number 10))
          )
      #f
      )
  )

(contains-palindrome? 10021)
                              