
squares :: [Int] -> [Int]
squares xs = [ x*x | x <- xs ]

odds :: [Int] -> [Int]
odds xs = [x | x <- xs, odd x]

sumSqOdd :: [Int] -> Int
sumSqOdd xs = sum [x*x | x <- xs, odd x]

main :: IO () -- main function for compilation
              -- IO indicates this is an action to handle IO sideffects
main = do -- execute things in sequence
         putStrLn("Sum of odd Squares from 1 to ?:")
         inp <- getLine           -- read any stdin line
         let x = read inp :: Int  -- and convert to integer
         print (sum (squares (odds [1..x])))
         print (sumSqOdd [1..x])
