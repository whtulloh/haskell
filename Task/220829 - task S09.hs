-- Jawaban dikirimkan ke
-- email: andy@emurgo.io
-- Subject: Haskell Task S09


-- COMPOSITE
-- --------------
-- TASK 1
-- --------------
-- Buatlah Function cekgt10 dengan argument List Int dan output list Int, 
-- lalu periksa apakah elemen dari list tersebut lebih besar dari 10, 
-- jika iya, maka elemen list tersebut di bagi 2, kalau tidak maka dibiarkan di dalam list
-- Pakai recursive dan pattern matching , semua variable harus di input dari argument (inputan user saat eksekuting)
-- Expected Result:
-- ghci> cekgt10 [1,10,12,3,8,90,5]
-- [1,10,6,3,8,45,5]

cekgt10 :: [Int] -> [Int]
cekgt10 [] = []
cekgt10 (x:xs)
        | x > 10 = x `div` 2 : cekgt10 xs
        | otherwise = x : cekgt10 xs

-- --------------
-- TASK 2
-- --------------
-- Buatlah Function selectMod0 dengan argument list Int dan output list Int, 
-- Kemudian pilih bilangan genap, sisanya dihilangkan.
-- Menggunakan function filter dan even
-- Expected Result:
-- ghci> selectMod0 [1,10,6,3,8,45,5]
-- [10,6,8]

selectMod0 :: [Int] -> [Int]
selectMod0 = filter even


-- --------------
-- TASK 3
-- --------------
-- Buatlah Function comFunc dengan argument list Int dan output list Int, 
-- Lalu pilih lagi hanya yg lebih besar dari 8 dan Susun dengan menggunakan composition (.)
-- Expected Result:
-- ghci> comFunc [10,6,8]
-- [10]

comFunc :: [Int] -> [Int]
comFunc = filter (\x -> x > 8)


-- LAMBDA
-- --------------
-- TASK 4
-- --------------
-- Buatlah Function isMod0 dengan argument Int dan output Bool, 
-- kemudian process apakah bisa dihabis dibagi 2. Menggunakan function mod.
-- Expected Result:
-- ghci> isMod0 4
-- True
-- ghci> isMod0 7
-- False

isMod0 :: Int -> Bool
isMod0 = (==0) . (\x -> x `mod` 2)


-- --------------
-- TASK 5
-- --------------
-- Buatlah Function listMod dengan argument list Int dan output list Int, 
-- Kemudian pilih hanya yg habis dibagi 2 saja yg ada di list, sisanya dihilangkan.
-- Menggunakan function filter dan isMod0.
-- Expected Result:
-- ghci> listMod [1,2,3,4,5,6,7,8,9,10]
-- [2,4,6,8,10]

listMod :: [Int] -> [Int]
listMod = filter isMod0


-- --------------
-- TASK 6
-- --------------
-- Buatlah Function listModlambda dangan logic sama dengan Task 5. 
-- Tapi perbedaannya menggunakan lambda
-- Expected Result:
-- ghci> listModlambda [1,2,3,4,5,6,7,8,9,10]
-- [2,4,6,8,10]

listModlambda :: [Int] -> [Int]
listModlambda = (\y -> filter isMod0 y)


-- --------------
-- TASK 7
-- --------------
-- Rubahlah Function nonlambda3 dengan menggunakan lambda.
-- Expected Result:
-- ghci> nonlambda3 2 3 4
-- 14
-- ghci> withlambda3 2 3 4
-- 14

nonlambda3 :: Integer -> Integer -> Integer -> Integer
nonlambda3 x y z = x + y * z

withlambda3 :: Int -> Int -> Int -> Int
withlambda3 = (\x y z -> x + y * z)