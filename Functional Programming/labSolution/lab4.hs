-- BTree

data BTree a = BLeaf a
             | BNode (BTree a) (BTree a)    deriving (Show)

btree_string :: (Show a) => BTree a -> String
btree_string (BLeaf n)    =  show n
btree_string (BNode l r)  =  (btree_string l) ++ "," ++ (btree_string r)

instance Functor BTree where
    -- Apply function to each node of the tree
    -- Works in all cases
    fmap f (BLeaf n)    =  BLeaf (f n)
    fmap f (BNode l r)  =  BNode (fmap f l) (fmap f r)

instance Applicative BTree where
    -- Assume function tree only has one function and apply that one to all values in the other tree
    -- Many variants possible, but no sensible definition
    pure = BLeaf
    (BLeaf f) <*> (BLeaf n)    =  BLeaf (f n)
    (BNode fl fr) <*> (BNode l r)  =  BNode (fl <*> l) (fr <*> r)

instance Monad BTree where
    -- Apply the function f :: a -> m b to each leaf
    -- Sensible, as we can apply the function f also to sub-trees
    -- Works in all cases
    return = BLeaf
    (BLeaf n)   >>= f  =  f n
    (BNode l r) >>= f  =  BNode (l >>= f) (r >>= f)

-- CTree

data CTree a = CNode a [CTree a]    deriving(Show)

ctree_string :: (Show a) => CTree a -> String -- assuming it's split at first child / binary tree
ctree_string (CNode n [])  =  show n
ctree_string (CNode n cs)  =  (ctree_string (head cs)) ++ "," ++ (show n) ++ "," ++ rest
                              where rest = foldr (\a b -> a ++ if b=="" then b else "," ++ b)
                                                 ""
                                                 [ctree_string c | c <- tail cs]

instance Functor CTree where
    -- Apply function to each node of the tree
    -- Works in all cases
    fmap f (CNode n cs)  =  CNode (f n) (map (fmap f) cs)

instance Applicative CTree where
    -- Matching a function tree with a value tree:
    -- * node has f n
    -- * children are every application of each function from the function tree to each element from the list of elements
    -- Works in all cases
    pure n  =  CNode n []
    (CNode f fs) <*> (CNode n cs)  =  CNode (f n) ((map (fmap f) cs) ++
                                                   (map (<*> (CNode n cs)) fs))

instance Monad CTree where
  -- Apply function to value to give new value and additional children and then bind function to all children
  -- Works in all cases
  return n = CNode n []
  CNode n cs >>= f  =  CNode n' (cs' ++ map (>>= f) cs)
                       where CNode n' cs' = f n

-- Example usage

main :: IO ()
main = do
          -- BTree demo
          putStrLn("# BTree")
          let btree = BNode (BNode (BNode (BLeaf 1) (BLeaf 2)) (BNode (BLeaf 3) (BLeaf 4))) (BNode (BNode (BLeaf 5) (BLeaf 6)) (BLeaf 7))
          print(btree)

          putStrLn("Print")
          print(btree_string btree)

          putStrLn("Functor: square values")
          print(btree_string (fmap (\x -> x*x) btree))

          putStrLn("Applicative (silly): combine with tree of functions mapping each node value x to x-2x")
          let bfunc = fmap (\x -> (\y -> x - 2*y)) btree
          print(btree_string(bfunc <*> btree))

          putStrLn("Monad: add 100 to each leaf; split leafs into div/mod 2")
          print(btree_string(btree >>= (\x -> BLeaf (x+100))))
          print(btree_string(btree >>= (\x -> BNode (BLeaf (x`div`2)) (BLeaf (x`mod`2)))))

          -- CTree demo
          putStrLn("\n# CTree")
          let ctree = CNode 4 [CNode 2 [CNode 1 [], CNode 3 []], CNode 6 [CNode 5 [], CNode 7 []]]
          print(ctree)

          putStrLn("Print")
          print(ctree_string ctree)

          putStrLn("Functor: square values")
          print(ctree_string (fmap (\x -> x*x) ctree))

          putStrLn("Applicative: (*2); ((*2) (+100) (+1000))")
          print(ctree_string((CNode (*2) []) <*> ctree))
          print(ctree_string((CNode (*2) [CNode (+100) [], CNode (+1000) []]) <*> ctree))

          putStrLn("Monad: add 100 and split into div/mod 2")
          print(ctree_string(ctree >>= (\x -> CNode (x+100) [])))
          print(ctree_string(ctree >>= (\x -> CNode (x+100) [CNode (x`div`2) [],CNode(x`mod`2) []])))
