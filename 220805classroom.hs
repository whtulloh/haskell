-- Recursive function 
natSum :: Int -> Int
natSum n = if n == 0 then 0
                else n + natSum (n-1)
-- natSum 3 -> expected 6 => 3 + 2 + 1
-- natSum 5 -> expected 15 => 5 + 4 + 3 + 2 + 1


-- Bedah function
head' :: [Int] -> [Int]
head' (x:xs) = [x]
-- head [1,2,3,4] -> expected 1
-- head' [1,2,3,4] -> expected [1] => result in list
tail' :: [Int] -> [Int]
tail' (x:xs) = xs
-- tail' [1,2,3,4] -> expected [2,3,4]


-- Pattern matching
repeatxNtimes :: Int -> a -> [a]
repeatxNtimes 0 x = []
repeatxNtimes n x = if n > 0 then x : repeatxNtimes (n - 1) x else []
-- repeatxNtimes 3 "HelloWorld" -> expected ["HelloWorld","HelloWorld","HelloWorld"]
-- repeatxNtimes 3 3 -> expected [3,3,3]
-- repeatxNtimes (-1) "Welcome" -> expected []


-- Recursive function to multiply until empty
allSquares :: [Int] -> [Int]
allSquares [] = []
allSquares (x : xs) = x * x : allSquares xs
-- allSquares [2,3,4] -> expected [4,9,16]


-- Recursive function to add by 10 until empty
tambah10 :: [Int] -> [Int]
tambah10 [] = []
tambah10 (x:xs) = (x + 10) : tambah10 xs
-- tambah10 [1,2,3,4] -> expected [11,12,13,14]


-- Function call other Function
add35 :: Int
add35 = 3 + 5

callAdd35 :: Int
callAdd35 = add35
-- callAdd35 -> expected 8


-- Call multiple Function
add2arg :: Int -> Int -> Int
add2arg x y = x + y

callAdd :: Int -> Int -> Int
callAdd = add2arg

callAddAlt :: Int -> Int -> Int
callAddAlt d g = add2arg d g

callAja :: Int
callAja = add2arg 4 3

mulThem :: Int -> Int -> Int
mulThem arg1 arg2 = arg1 * arg2

callMoreThanOneFunction :: Int
callMoreThanOneFunction = let addit = callAja
                          in mulThem addit 2
-- callMoreThanOneFunction -> ecpected 14
-- callAja -> expected 7
-- mulThem addit 2 -> expected 14 => addit = callAja = 7


-- Bedah function Hanoi Tower
hanoi :: Int -> a -> a -> a -> [(a,a)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b++ [(a,b)] ++ hanoi (n-1) c b a
-- hanoi 3 1 2 3 -> expected [(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2)]
-- hanoi 4 1 2 3 -> expected [(1,3),(1,2),(3,2),(1,3),(2,1),(2,3),(1,3),(1,2),(3,2),(3,1),(2,1),(3,2),(1,3),(1,2),(3,2)]
-- 1 2 3 mendefinisikan tower


-- Where inside Function
jumlah :: Int -> Int -> Int
jumlah a b = arg1 + arg2
                where 
                        arg1 = a + 1
                        arg2 = b - 2
-- jumlah 5 4 -> expected 8 => 5+1 = 6, 4-2 = 2, 6+2 = 8


-- Bedah sorting function
qsort1 :: Ord a => [a] -> [a]
qsort1 [] = []
qsort1 (p:xs) = qsort1 lesser ++ [p] ++ qsort1 greater
                where
                        lesser = filter (<p) xs
                        greater = filter (>=p) xs
-- qsort1 [200, 500, 50, 20, 400] -> expected [20,50,200,400,500]