
------------task1------------
histogram :: Eq t => [t] -> [(t,Int)]
histogram [] = []
histogram lst@(x:_) = (x, length $ filter (==x) lst) : histogram filter (/=x) lst --lst  is alias to (x:xs) but we still need to pattern match by elements to check x
  --where (equals, unequals) = partition (==x) lst -- separates a list into two lists based on the predicate
 
maximumBy :: (Eq b, Ord b) => (a -> b) -> [a] -> a
maximumBy criteria xs = foldl1 (maxBy criteria) xs --foldl1 is like foldl but nv is taken as the first element of the list (only for lists  with at least 1 element)
  where maxBy :: Eq a => (a -> b) -> a -> a -> a
        maxBy criteria x y
          | (criteria x) < (criteria y) = y
          | otherwise = x

mostFrequents :: (Eq t,Num t) => [t] -> [t]
mostFrequents xs = map fst (filter (\(x,frequency) -> frequency >= maxFrequency) hist) -- map fst $ filter ...
  where hist = histogram xs
        (x, maxFrequency) = maximumBy snd hist
    
intersect :: Eq t => [t] -> [t] -> [t]
intersect xs ys = filter (`elem` ys) xs

intersection :: Eq t => [[t]] -> [t]
intersection [] = []
intersection xs = foldl1 intersect xs
    
mostFrequent :: (Eq t, Num t) => [[t]] -> t
mostFrequent [] = 0
mostFrequent xs
  | null (intersection xs)= 0
  | otherwise = head (intersection xs)
      
------------task3a------------
type TvShow = (String, (Int, Int), Int)

minToEnd :: TvShow -> Int
minToEnd (_,(hours,minutes),duration) = hours * 60 + minutest + duration

lastShow :: [TvShow] -> TvShow
lastShow shows = maximumBy minToEnd shows

------------task3b------------
duration :: TvShow -> Int
duration (_,_,x) = x

programDuration :: [tvShow] -> Int
programDuration shows = sum (map duration show)

candidates :: [TvShow] -> [TvShow]

subsets :: [t] -> [[t]]
subsets [] = [[]]
subsets (x:xs) = noX ++ yesX
  where noX = subsets xs
        yesX = map (x:) noX

longestProgram :: [TvShow] -> [TvShow]
longestProgram shows = maximumBy programDuration (candidates shows)