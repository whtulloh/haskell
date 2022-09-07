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
    toList a = a
-- ghci> toList [1,2,3::Int]
--  --> expected [1,2,3]

data Tree a = Empty | Node a (Tree a) (Tree a)

foo x y = sum (toList x) == sum (toList y) || x < y

-- instance Listable Tree a where
