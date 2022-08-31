-- Function with idiomatic way
foobar :: [Int] -> Int
foobar [] = 0
foobar (x:xs) 
            | x > 3 = (7*x + 2) + foobar xs
            | otherwise =  foobar xs
-- foobar [1,10,12,3,8,90,5] -> expected 885

foobar' :: [Int] -> Int
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)
-- foobar' [1,10,12,3,8,90,5] -> expected 885

cekgt10 :: [Int] -> [Int]
cekgt10 = map (\x -> x `div` 2) . filter (>10)
-- cekgt10 [1,10,12,3,8,90,5] -> expected [6,45]

fun1 :: [Int] -> Int
fun1 [] = 1
fun1 (x:xs)
        | even x = (x-2) * fun1 xs
        | otherwise = fun1 xs
-- fun1 [1,4,3] -> expected 2
-- fun1' xs = (*1) . map (\x xs -> x-2) . filter even

xor :: [Bool] -> Bool
xor = foldl (\x y -> (x || y) && not (x && y)) False
