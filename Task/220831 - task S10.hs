-- --------------
-- TASK 1
-- --------------

-- fun1 :: [Int] -> Int
-- fun1 [] = 1
-- fun1 (x:xs)
--      | even x = (x - 2) * fun1 xs
--      | otherwise = fun1 xs

-- Tugas bikin function yg sama dengan code diatas tapi jangan pakai pattern matching, jangan pakai if, jangan pakai case, jangan pakai guards, buat hanya dengan satu line dan dengan beberapa function prelude yg ada disini misalnya:
-- lambda dan fungsi2 seperti composition (.), filter / takewhile, map, product , even
-- Expected Result:
-- ghci> fun1 [10,20,13]      
-- 144
-- ghci> fun1b [10,20,13]      
-- 144
-- ghci> fun1b [10,20,30]
-- 4032

fun1 :: [Int] -> Int
fun1 [] = 1
fun1 (x:xs)
     | even x = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1b :: [Int] -> Int
fun1b = (product) . map (\x -> x-2) . filter (even)


-- --------------
-- TASK 2
-- --------------

-- fun2 :: Int -> Int
-- fun2 1 = 0
-- fun2 n | even n = n + fun2 (n `div` 2)
--              | otherwise = fun2 (3 * n + 1)
-- Ubah fungsi diatas dengan lambda, iterate, takewhile, filter, sum, composition, even dan if then else atau where dll
-- Expected Result:
-- ghci> fun2 5
-- 30
-- ghci> fun2b 5
-- 30

fun2 :: Int -> Int
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

help1 :: Int -> Int
help1 n = if even n then n `div` 2 else 3 * n + 1

help2 :: Int -> [Int]
help2 1 = []
help2 n = n : help2 (help1 n)

fun2b :: Int -> Int
fun2b = sum . filter even . (help2)

-- Exclusive OR by Pak Sony
xor :: [Bool] -> Bool
xor = foldl (\x y -> (x || y) && not (x && y)) False

-- Map as a Fold
maps' :: (a -> b) -> [a] -> [b] 
maps' func = foldr (\x xs -> func x : xs) []