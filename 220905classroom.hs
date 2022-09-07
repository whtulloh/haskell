tambahInt :: Int -> Int -> Int
tambahInt a b = a + b

tambahFloat :: Float -> Float -> Float
tambahFloat a b = a + b

tambah a b = a + b
-- tambah 1.5 5 -> expected 6.5
-- tambah 2 5 -> expected 7

tambah' :: Num a => a -> a -> a
tambah' a b = a + b
-- tambah' 1.5 5 -> expected 6.5
-- tambah' 2 5 -> expected 7

f1 :: a -> a -> a
f1 x y = x
-- f1 4 5 -> expected 4

f2 :: a -> a -> a
f2 x y = y
-- f2 'a' 'b' -> expected 'b'

-- Function Error
-- f3 :: jaya -> jaya -> jaya
-- f3 a b - a + b
f3 :: jaya -> jaya -> jaya
f3 x y = x
-- f3 "Ya" "Tidak" -> expected "Ya"

data Foo = F Int | G Char | X Int Char
    deriving Show
-- F 100 :: Foo -> expected F 100
-- F 100 -> expected F 100

instance Eq Foo where
    (F a1) == (F a2) = a1 == a2
    (G c1) == (G c2) = c1 == c2
    (X angka1 huruf1) == (X angka2 huruf2) = (angka1 == angka2) && (huruf1 == huruf2)
    _ == _ = False
-- ghci> x = F 100
-- ghci> y = F 200
-- ghci> x == y   
-- -> expected False
-- ghci> a = G 'a'
-- ghci> b = G 'b'
-- ghci> a == b
-- -> expected False
-- ghci> x == b
-- -> expected False
-- ghci> p = X 100 'a'
-- ghci> q = X 100 'a'
-- ghci> p == q
-- -> expected True

data Temperatur = C Float | Fh Float
    deriving (Show, Ord)
-- ghci> a = C 100
-- ghci> b = C 150
-- ghci> c = Fh 212
-- ghci> d = Fh 150
-- ghci> a == d
-- -> expected False
-- ghci> a == b
-- -> expected False
instance Eq Temperatur where
    (C c1) == (C c2) = c1 == c2
    (Fh f1) == (Fh f2) = f1 == f2
    (C c) == (Fh f) = (c * 9/5) + 32 == f
    (Fh f) == (C c) = (f-32) * 5/9 == c
    _ == _ = False