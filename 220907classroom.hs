{-# LANGUAGE FlexibleInstances #-}

class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    toList a = [a]
-- ghci> toList (10 :: Int)
-- --> expected [10]

instance Listable Bool where
    toList True = [1]
    toList False = [0]
-- instance Listable Bool where
--     toList a
--         | a == True = [1]
--         | otherwise = [0]
-- ghci>  toList True
-- --> expected [1]
-- ghci>  toList False
-- --> expected [0]

instance Listable ([Int]) where
  toList xs = xs
--   toList xs = id
-- > toList [1,2,3::Int]
-- [1,2,3]

instance Listable (Tree Int) where
  toList Empty = []
  toList (Node element left right) = (toList left) ++ [element] ++ (toList right)
-- > toList sampleTree
-- [2, 1, 3]

data Tree a = Empty | Node a (Tree a) (Tree a)
   deriving Show

-- sample data 
sample = 123

sampleTree :: Tree Int
sampleTree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)