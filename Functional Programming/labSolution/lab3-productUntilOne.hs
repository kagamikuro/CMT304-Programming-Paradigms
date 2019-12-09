-- productUntilOne (two versions provided)

productUntilOne :: IO Integer
productUntilOne = do
                    s <- getLine;
                    let x = read s :: Integer in
                      if x == 1 then
                        return 1
                      else do
                        xs <- productUntilOne;
                        return (x * xs)

productUntilOne' :: IO Integer
productUntilOne' =  getLine >>= \s ->
                      let x = read s :: Integer in
                        if x == 1 then
                          return 1
                        else
                          productUntilOne' >>= \xs -> return (x * xs)

main :: IO ()
main = productUntilOne' >>= \x -> putStrLn (show x) -- either version is OK here

