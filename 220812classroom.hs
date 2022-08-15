-- Prelude Date Time
import Data.Time.Clock.POSIX (getPOSIXTime)
-- ghci > getPOSIXTime -> expected: 1660572920.0370908s --> epoch formated date
-- ghci > (round) <$> getPOSIXTime -> expected: 1660572948 --> epoch formated rounded
import Data.Time
-- ghci> getZonedTime -> expected: 2022-08-15 21:16:57.0385638 SE Asia Standard Time
-- ghci> getCurrentTime -> expected: 2022-08-15 14:17:03.6223363 UTC
-- ghci> formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" <$> getZonedTime -> expected: "2022/08/15 21:18:27"
-- ghci> formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" <$> getCurrentTime -> expected: "2022/08/15 14:20:13"

-- Data type InList
data IntList = Empty | Cons Int IntList deriving Show

-- Function add 1 to all variable
addOnetoAll :: IntList -> IntList
addOnetoAll Empty = Empty
addOnetoAll (Cons x xs) = Cons (x+1) (addOnetoAll xs)

-- Prelude function abs, absolute change to positive integer
-- ghci > abs 10 -> expected: 10
-- ghci > abs (-123) -> expected: 123 
-- ghci > :t abs -> expected: abs :: Num a => a -> a -- Num can be Integer, Fload, Double

-- Absolute All
absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

-- square All
squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

-- Contoh penggunaan
myIntList = Cons 2 (Cons (-3) (Cons 5 Empty))
-- addOnetoAll myIntList -> expected: Cons 3 (Cons (-2) (Cons 6 Empty))
-- absAll myIntList -> expected: Cons 2 (Cons 3 (Cons 5 Empty))
-- squareAll myIntList -> expected: Cons 4 (Cons 9 (Cons 25 Empty))

-- Map Int List untuk membuat general function dari addOnetoAll, absAll and squareAll
mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList func (Cons x xs) = Cons (func x) (mapIntList func xs)

addOne :: Int -> Int
addOne x = x + 1

square :: Int -> Int
square x = x * x

myMapInList = Cons 2 (Cons (-3) (Cons 5 Empty))
-- mapIntList addOne myMapInList -> expected: Cons 3 (Cons (-2) (Cons 6 Empty))
-- mapIntList square myMapInList -> expected: Cons 4 (Cons 9 (Cons 25 Empty))
-- mapIntList abs myMapInList -> expected: Cons 2 (Cons 3 (Cons 5 Empty))

-- Prelude function even odd
-- ghci > even 6 -> expected: True
-- ghci > :t even -> expected: even :: Integral a => a -> Bool -- Integral can be only Integer
-- ghci > odd 7 -> expected: False
-- ghci > :t even -> expected: odd :: Integral a => a -> Bool -- Integral can be only Integer

-- keep the positive and take out the negative
keepOnlyPositive :: IntList -> IntList
keepOnlyPositive Empty = Empty
keepOnlyPositive (Cons x xs)
                    | x > 0 = Cons x (keepOnlyPositive xs)
                    | otherwise = keepOnlyPositive xs
-- keepOnlyPositive myMapInList -> expected: -- Cons 2 (Cons 5 Empty)

-- keep only even and take out odd
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
                    | even x = Cons x (keepOnlyEven xs)
                    | otherwise = keepOnlyEven xs
-- keepOnlyEven myMapInList -> expected: Cons 2 Empty

-- polymorphism, sebuah function dinamis yang bisa digunakan lebih dari 1 variable
data List t = E | C t (List t) deriving Show

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
                | p x = C x (filterList p xs)
                | otherwise = filterList p xs

myOtherList = C 2 (C (-3) (C 5 E))

mapOtherList :: (a -> b) -> List a -> List b
mapOtherList f E = E
mapOtherList f (C x xs) = C (f x) (mapOtherList f xs)

double :: Int -> Int
double x = 2 * x

main = do 
    print (filterList even myOtherList)
    print (filterList odd myOtherList)
    print (mapOtherList double myOtherList)
-- main
-- C 2 E
-- C (-3) (C 5 E)
-- C 4 (C (-6) (C 10 E))

addme :: Int -> Int -> Int
addme arg1 arg2 = arg1 + arg2
-- addme 1 2 -> expected: 3
-- addme 1 3 -> expected: 4

addme' :: Num a => a -> a -> a
addme' arg1 arg2 = arg1 + arg2
-- addme' 1 2 -> expected: 3
-- addme' 1 3 -> expected: 4