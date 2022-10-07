data MaybeSaya a = JustSaya a | NothingSaya
                deriving Show

instance Functor MaybeSaya where
    fmap fungsi (JustSaya a) = JustSaya (fungsi a)
    fmap fungsi NothingSaya  = NothingSaya

-- > fmap (*3) (JustSaya 10)
-- > JustSaya 30
-- > (*3) <$> (Just2 5)
-- > Just2 15

data Tree a = Tip a | Branch (Tree a) (Tree a)
    deriving Show

instance Functor Tree where
    fmap func (Tip a) = Tip (func a)
    fmap func (Branch left right) = Branch (fmap func left) (fmap func right)

-- > fmap (*3) (Branch (Tip 5) (Branch (Tip 4) (Tip 8)))
-- > Branch (Tip 15) (Branch (Tip 12) (Tip 24))