-- --------------
-- TASK 1
-- --------------
-- apa type data dari data dibawah ini ?  
-- A.   ['a','b','c'] :: [Char]
-- B.   ('a','b','c') :: (Char, Char, Char)
-- C.   [(False,'0'),(True,'1')]  :: [(Bool, Char)]
-- D.   ([False, True],['0','1']) :: ([Bool], [Char])
-- E.   [tail, init, reverse] :: [[a] -> [a]]

-- --------------
-- TASK 2
-- --------------
-- Apa type / signature dari fungsi dibawah ini ?
-- A.  
-- second :: [a] -> a
-- second xs = head (tail xs)

-- B.
-- swap :: (a,b) -> (b,a)
-- swap (x,y) = (y,x)

-- C.  
-- pair :: a -> b -> (a,b)
-- pair x y = (x,y)

-- D.  
-- double :: Num a => a -> a
-- double x = x * 2

-- E.  
-- palindrome :: (==) a => [a] -> Bool
-- palindrome xs = reverse xs == xs

-- F.  
-- twice :: (a -> a) -> a -> a
-- twice       f        x =  f (f x)

-- --------------
-- TASK 3
-- --------------
-- class Listable a where
--   toList :: a -> [Int]
  
-- instance Listable Int where
--   toList x = [x]

-- instance Listable Bool where
--   toList True  = [1]
--   toList False = [0]

-- Contoh:
-- mainInt :: IO ()
-- mainInt = print (toList (7::Int))

-- Expected Result:
-- ghci> mainInt
-- [7]

-- ----------
-- TASK 3.1
-- ----------
-- mainBool :: IO ()
-- mainBool = print (toList True, toList False)

-- Expected Result:
-- ghci> mainBool
-- ([1],[0])

-- ----------
-- TASK 3.2
-- ----------
-- mainList :: IO ()
-- mainList = print (toList True, toList (7::Int))

-- Expected Result:
-- ghci> mainList
-- ([1],[7])

-- ----------
-- TASK 3.3
-- ----------
-- mainInt2 :: Int -> IO ()
-- mainInt2 x = print (toList (x))

-- Expected Result:
-- ghci> mainInt2 53
-- [53]