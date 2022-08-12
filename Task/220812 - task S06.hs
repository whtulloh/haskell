-- -----------------------------
-- Task 1
-- -----------------------------
-- Buat sebuah function untuk memfilter sebuah list dengan menggunakan filter odd 
-- Prelude function
-- ghci> filter odd [1,2,3,4,5,6,7]
-- [1,3,5,7]
-- Expected Result:
-- >keepOnlyOddValue odd [1,2,3,4,5,6,7]
-- [1,3,5,7]
keepOnlyOddValue :: (Int -> Bool) -> [Int] -> [Int]
keepOnlyOddValue _ [] = []
keepOnlyOddValue func (x:xs)
                | func x = x : keepOnlyOddValue func xs
                | otherwise = keepOnlyOddValue func xs


-- -----------------------------
-- Task 2
-- -----------------------------
-- Buatlah function yg bisa menjalankan pemilihan odd dan even, 
-- argument “odd” yg keluar adalah yg ganjil dan jika masukan “even” yg keluar genap 
-- Expected Result:
-- ghci> filterValue "even" [1,2,3,4,5,6,7,8]
-- [2,4,6,8]
-- ghci> filterValue "odd" [1,2,3,4,5,6,7,8]    
-- [1,3,5,7]
filterValue :: String -> [Int] -> [Int]
filterValue _ [] = []
filterValue s (x:xs)
                | s == "even" && even x == True = x : filterValue s xs
                | s == "odd" && odd x == True = x : filterValue s xs
                | otherwise = filterValue s xs

-- -----------------------------
-- Task 3
-- -----------------------------
-- Buatlah fungsi yg sama dengan map
-- Prelude function
-- ghci> map (+10) [1,2,3,4,5]
-- [11,12,13,14,15]
-- (+10) adalah sebuah function
-- Expected Result:
-- Hasilnya akan sama saat function dan list yg sama diaplikasikan dalam map
-- Misalnya 
-- ghci> accumulateRec (+10) [1,2,3,4,5]
-- [11,12,13,14,15]
accumulateRec :: (Int -> Int) -> [Int] -> [Int]
accumulateRec _ [] = []
accumulateRec f (x:xs) = f(x) : accumulateRec f xs





