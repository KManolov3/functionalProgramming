fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

f :: [Integer] -> Integer
f [] = 5
f (x:xs) = x

null' :: [a] -> Bool
null' [] = True
null' xs = False

head' :: [a] -> a
head' [] = error "Prazen spisak brat"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = error "no value here"
tail' (x:xs) = xs

take' :: Integer -> [a] -> [a]
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x:(take' (n-1) xs)

drop' :: Integer -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (x:xs) = (drop' (n-1) xs)

(!) :: [a] -> Integer -> a
(!) xs n = head' (drop' n xs)

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

magic = 1 : 1 : zipWith (+) (tail magic) magic

last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs

allButLast' :: [a] -> [a]
allButLast' (x:[]) = []
allButLast' (x:xs) = x:(allButLast' xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = last' xs : reverse' (allButLast' xs)

map' :: (a -> a) -> [a] -> [a]
map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

apply' :: (a -> b) -> a -> b
apply' f a = (f a)

applyTwice :: (a -> a) -> a -> a
applyTwice f a = (f (f a))

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) 
    | (f x)     = x : (filter' f xs)
    | otherwise =(filter' f xs)

