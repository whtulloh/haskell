half x = if even x
         then Just (x `div` 2)
         else Nothing

half' x = if even x
         then Just2 (x `div` 2)
         else Nothing2

-- > (Just 4) >>= half
-- > Just 2
-- > (Just 4) >>= half >>= half
-- > Just 1
-- > (Just 4) >>= half >>= half >>= half
-- > Nothing

data Maybe2 a = Just2 a | Nothing2
                deriving Show

instance Functor Maybe2 where
    fmap f (Just2 x) = Just2 (f x) 
    fmap f Nothing2 = Nothing2

instance Applicative Maybe2 where
    pure = Just2
    Just2 f <*> (Just2 val) = Just2 (f val)
    Nothing2 <*> (Just2 val) = Nothing2

instance Monad Maybe2 where
    Just2 val >>= f = f val
    Nothing2 >>= f = Nothing2

-- > (Just2 4) >>= half'
-- > Just2 2
-- > (Just2 4) >>= half' >>= half'
-- > Just2 1
-- > (Just2 4) >>= half' >>= half' >>= half'
-- > Nothing2

data Tree a = Tip a | Branch (Tree a) (Tree a)
    deriving Show

tree = (Branch (Tip 4) (Branch (Tip 5) (Tip 6)))
-- tree = Tip 4

func x
    | x == 4 = Tip 99
    | otherwise = Branch (Tip (x * 2)) (Tip (x * 3))

instance Functor Tree where
    fmap f (Tip a) = Tip (f a)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
    pure = Tip
    Tip f <*> b = fmap f b
    (Branch l r) <*> b = Branch (l <*> b) (r <*> b)

instance Monad Tree where
    Tip val >>= f = f val
    (Branch l r) >>= f = Branch (l >>= f) (r >>= f)