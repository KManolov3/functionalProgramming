factorial :: Int -> Int
factorial 1 = 1
factorial n = n*factorial(n-1)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib(n-2)

addVectors :: (Int, Int) -> (Int, Int)->(Int, Int)
addVectors (a1, a2) (b1, b2) = (a1+b1, a2+b2)

compress :: Eq a => [a] -> [a]
compress (a:b:hs) | a==b      = compress(b:hs)
                  | otherwise = a:compress(b:hs)
compress hs = hs

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (a:hs) = a:a:duplicate hs

cycle' :: [a] -> [a]
cycle' l = [elem |y <-[1..], elem <- l]

cycle'' :: [a] -> [a]
cycle'' [] = []
cycle'' xs = iter xs
    where iter [] = iter xs
          iter (x:xs1) = (x : iter xs1)

quickSort:: Ord a => [a] -> [a]
quickSort [] = []
quickSort (pivot:rest) = quickSort smallerOrEqual ++ (pivot : quickSort bigger)
    where smallerOrEqual = filter (\el -> el<= pivot) rest
          bigger = filter (\el -> el>pivot) rest

mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where mid = length xs `quot` 2
        (firstHalf, secondHalf) = splitAt mid xs
        merge [] xs = xs
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | y < x = y : merge (x:xs) ys
          | otherwise = x : merge xs (y:ys)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' (x:xs) = foldl (flip (:)) [x] xs

iterate' :: (a->a) -> a -> [a]
iterate' f x = (f x):(iterate' f (f x))