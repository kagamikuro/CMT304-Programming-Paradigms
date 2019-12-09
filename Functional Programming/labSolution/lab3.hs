-- WhileIO (two versions provided)

whileIO :: Read a => (a -> a -> a) -> (a -> Bool) -> a -> IO a
whileIO op cnd trm =  do
                        s <- getLine;
                        let x = read s in
                          if (cnd x) then
                            return trm
                          else do
                            xs <- whileIO op cnd trm;
                            return (op x xs)

whileIO' :: Read a => (a -> a -> a) -> (a -> Bool) -> a -> IO a
whileIO' op cnd trm = getLine >>= \s ->
                        let x = read s in
                          if (cnd x) then
                            return trm
                          else
                            whileIO' op cnd trm >>= \xs -> return (op x xs)

