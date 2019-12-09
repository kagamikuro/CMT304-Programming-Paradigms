-- Pythagorean Triples
pythagorean_triple :: Int -> Int -> Int -> Bool
pythagorean_triple a b c = (a*a + b*b - c*c) == 0

-- Half-Evens
half_evens :: [Int] -> [Int]
half_evens []   = []
half_evens (x:xs)
    | even x    = x `div` 2 : half_evens xs
    | otherwise = half_evens xs

-- Dot Product
dot_product :: [Int] -> [Int] -> Int
dot_product [] []         = 0
dot_product (x:xs) (y:ys) = x*y + dot_product xs ys

-- Binary Sequence
binary_seq :: Int -> [[Int]]
binary_seq n = iterate bits [[]] !! n
  where bits seqs = [b : seq | seq <- seqs, b <- [0,1]]

