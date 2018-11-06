-- comment

{-
	-comment
	-comment
-}

-- if True then 1 else 2

a = 5

-- a = 10 - error

f x = 1

succ' x = x + 1

evenOrOdd x = rem x 2 == 0

factoriel x = if x <= 1 then x else x*(factoriel (x-1))

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

(+++++) 0 y = y
(+++++) x y = (+++++) (pred x) (succ y)

sumDivisors i n accum 
    | i == n = accum
	| mod n i == 0 = sumDivisors (i + 1) n (accum + i)
    | otherwise    = sumDivisors (i + 1) n accum
					
					

