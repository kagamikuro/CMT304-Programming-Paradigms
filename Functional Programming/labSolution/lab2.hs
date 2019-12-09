-- Reverse
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- RemoveAt (two versions provided)
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = ((xs !! n), [ x | (i, x) <- zip [0..] xs, i /= n ])

removeAt' :: Int -> [a] -> (Maybe a, [a]) -- safe (using Just/Nothing)
removeAt' _ [] = (Nothing, [])
removeAt' 0 (x:xs) = (Just x, xs)
removeAt' k (x:xs) = let (a, r) = removeAt' (k - 1) xs in (a, x:r)

-- Combinations
combs :: Int -> [a] -> [[a]]
combs 0 _  = [ [] ]
combs n xs = [ xs !! i : x | i <- [0..(length xs)-1],
                             x <- combs (n-1) (drop (i+1) xs) ]

-- Eight Queens (two versions provided)
queens :: Int -> [[Int]]
queens n = filter test (generate n)
    where generate 0      = [[]]
          generate k      = [q : qs | q <- [1..n], qs <- generate (k-1)]
          test []         = True
          test (q:qs)     = isSafe q qs && test qs
          isSafe   try qs = not (try `elem` qs || sameDiag try qs)
          sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs

queens' :: Int -> [[Int]]
queens' n = map rev $ queensh n
    where queensh 0       = [[]]
          queensh k       = [q:qs | qs <- queensh (k-1), q <- [1..n], isSafe q qs]
          isSafe   try qs = not (try `elem` qs || sameDiag try qs)
          sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs

