cartesian :: [a] -> [b] -> [(a,b)]
cartesian xs ys = [(x,y) | x<-xs , y <-ys]

iterate' :: (a->a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

nats' :: [Integer]
nats' = iterate' succ 0

divides :: Integer -> Integer -> Bool
divides x y = mod y x == 0

divisors :: Integer -> [Integer]
divisors x = [y | y <- [1..x], mod x y == 0]

isPrime :: Integer -> Bool
isPrime x = divisors x == [1, x]

primes :: [Integer]
primes = [y | y <- [1..], (length (divisors y)) == 2]

eratosthenes :: [Integer] -> [Integer]
--eratosthenes xs = [y | y <- (filter (\x -> divides y x == False) xs)]

eratosthenes (x:xs) = x : eratosthenes (filter (\y -> not (divides x y)) xs)

positive2Tuples :: [(Integer, Integer)]
positive2Tuples = [x | i <- [1..], x <- partitionPos i]

partitionPos :: Integer -> [(Integer, Integer)]
partitionPos n = [(x,y) | x <- [1..n-1], y <- [n-x]]

fact :: Integer -> Integer
fact 1 = 1
fact x = x * (fact (x-1))

facts :: [Integer]
facts = [x | i <- [1..], x <- [fact i]]

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs :: [Integer]
--fibs = [fib i | i <- [1..]]
fibs = map fib [0..]

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f nv []     = nv
foldl' f nv (x:xs) = foldl' f (f nv x) xs

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f nv []     = [nv]
scanl' f nv (x:xs) = nv : scanl' f (f nv x) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] ys         = []
zip' xs []         = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

zipWith' :: [a] -> [b] -> (a -> b -> b) -> [b]
zipWIth' [] ys f         = []
zipWith' xs [] f         = []
zipWith' (x:xs) (y:ys) f = (f x y) : zipWith' xs ys f

fact' n = (scanl (\ x y -> x * y) 1 [1..n])

facts' :: [Integer]
facts' = map fact'	 [1..]

