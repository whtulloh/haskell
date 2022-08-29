-- Find value more than 100
gt100 :: Int -> Bool
gt100 x = x > 100
-- gt100 99 -> expected False
-- gt100 101 -> expected True

greaterThan100 :: [Int] -> [Int]
greaterThan100 xs = filter gt100 xs
-- greaterThan100 [10,100,200,99,50] -> expected [200]

-- Lamda as a function
greaterThan1002Two :: [Int] -> [Int]
greaterThan1002Two xs = filter (\x -> x > 100) xs
-- greaterThan1002Two [10,100,200,99,50] -> expected [200]

-- Lamda more than one param
-- ghci> (\x y z -> [x, y*2, z*3]) 1 2 3 -> expected [1,4,9]
-- ghci> filter (>100) [50,101,99,200] -> expected [101,200]
-- ghci> (>100) 102 -> expected True
-- ghci> (100>)102 -> expected False

-- composition function
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (.) f g = \x -> f (g x)
-- Prelude
-- ghci> (+) 1 2 -> expected 3

myTest :: [Int] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Int] -> Bool
myTest' = even . length . greaterThan100

-- Curried functions
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
-- ghci> let multTwoWithNine = multThree 9  
-- ghci> multTwoWithNine 2 3 -> expected 54
-- ghci> let multWithEighteen = multTwoWithNine 2
-- ghci> multWithEighteen 10 -> expected 180

-- Prelude
-- ghci> 1 `elem` [1,2,3,4,5] -> expected True