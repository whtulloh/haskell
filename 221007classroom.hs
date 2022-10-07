data Maybe2 a = Just2 a | Nothing2
    deriving Show

instance Functor Maybe2 where
    fmap f (Just2 a) = Just2 (f a)
    fmap f Nothing2 = Nothing2

instance Applicative Maybe2 where
    pure = Just2
    -- Just2 f <*> (Just2 a) = Just2 (f a)
    -- Nothing2 <*> (Just2 a) = Nothing2
    Just2 f <*> j = fmap f j
    Nothing2 <*> j = Nothing2

-- > x = Just2 5
-- > y = Just2 (*3)
-- > y <*> x
-- > Just2 15
-- > (Just2 (*3)) <*> (Just2 5)
-- > Just2 15
-- > (*) <$> (Just2 5) <*> (Just2 5)
-- > Just2 25
-- > [(*2), (+3)] <*> [4,5,6]
-- > [8,10,12,7,8,9]

data Tree a = Tip a | Branch (Tree a) (Tree a)
    deriving Show

tree = (Branch (Tip 5) (Branch (Tip 4) (Tip 8)))

instance Functor Tree where
    fmap f (Tip a) = Tip (f a)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
    pure = Tip
    Tip f <*> b = fmap f b
    (Branch l r) <*> b = Branch (l <*> b) (r <*> b)

-- > (*3) <$> (Branch (Tip 5) (Branch (Tip 4) (Tip 8)))
-- > Branch (Tip 15) (Branch (Tip 12) (Tip 24))
-- > Tip (*3) <*> tree
-- > Branch (Tip 15) (Branch (Tip 12) (Tip 24))
-- > Branch (Tip (+1)) (Tip (*2)) <*> tree
-- > Branch (Branch (Tip 6) (Branch (Tip 5) (Tip 9))) (Branch (Tip 10) (Branch (Tip 8) (Tip 16)))
