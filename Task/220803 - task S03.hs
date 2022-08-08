import System.Win32 (xBUTTON1)
-- Masukan sebuah list dari nomor credit card sbb [4,5,5,6,7,3,7,5,8,6,8,9,9,8,5,5]

-- -----------------------------
-- Task 1
-- -----------------------------
-- Hapus digit terakhir dari nomor cc tersebut, dan simpan nomor yg dihapus itu 
-- sebagai variable check digit / atau function baru utk mengambil digit terakhir
-- Run: dropLast [4,5,5,6,7,3,7,5,8,6,8,9,9,8,5,5]
-- Expected dropLast:  [4,5,5,6,7,3,7,5,8,6,8,9,9,8,5]
-- dropLast :: [Int] -> [Int]
-- ???
dropLast :: [Int] -> [Int]
dropLast [] = []
dropLast a = take ((length a)-1) a
-- ATAU
-- dropLast :: [Int] -> [Int]
-- dropLast [] = []
-- dropLast [x] = []
-- droplast (x:xs) = x:dropLast xs

-- Run: checkDigit [4,5,5,6,7,3,7,5,8,6,8,9,9,8,5,5]
-- Expected checkDigit:  5
-- checkDigit :: [Int] -> Int
-- ???
checkDigit :: [Int] -> Int
checkDigit [] = 0
checkDigit a = last a
-- ATAU
-- checkDigit :: [Int] -> Int
-- checkDigit [x] = x
-- checkDigit (_:xs) = checkDigit xs

-- -----------------------------
-- Task 2
-- -----------------------------
-- Dari result diatas, Reverse list tersebut (balik urutannya)
-- Run: reverseList [4,5,5,6,7,3,7,5,8,6,8,9,9,8,5]
-- Expected Result: [5,8,9,9,8,6,8,5,7,3,7,6,5,5,4]
-- reverseList :: [Int] -> [Int]
-- ???
reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList a = reverse a
-- ATAU
-- reverseList :: [Int] -> [Int]
-- reverseList [] = []
-- reverseList (x:xs) = reverseList xs ++ [x]

-- -----------------------------
-- Task 3
-- -----------------------------
-- Kalikan digit di posisi ganjil (Result diatas) dengan angka 2
-- Run: multiplyOddPlacesby2 [5,8,9,9,8,6,8,5,7,3,7,6,5,5,4]
-- Expected Result: [10,8,18,9,16,6,16,5,14,3,14,6,10,5,8]
-- multiplyOddPlacesby2 :: [Int] -> [Int]
-- ???
multiplyOddPlacesby2 :: [Int] -> [Int]
multiplyOddPlacesby2 [] = []
multiplyOddPlacesby2 [x] = [2*x]
multiplyOddPlacesby2 (x:y:xs) = (2*x):y: multiplyOddPlacesby2 xs

-- -----------------------------
-- Task 4
-- -----------------------------
-- Dari result diatas, Kurangi dengan 9 semua nomor yg nilainya lebih besar dari 9
-- Run: subtract9 [10,8,18,9,16,6,16,5,14,3,14,6,10,5,8]
-- Expected Result: [1,8,9,9,7,6,7,5,5,3,5,6,1,5,8]
-- subtract9 :: [Int] -> [Int]
-- ???
subtract9 :: [Int] -> [Int]
subtract9 [] = []
subtract9 (x : xs)
                | x > 9 = x - 9 : subtract9 xs
                | otherwise = x :  subtract9 xs

-- -----------------------------
-- Task 5
-- -----------------------------
-- Dari result diatas, Jumlahkan semua angka yg ada
-- Run: addAll [1,8,9,9,7,6,7,5,5,3,5,6,1,5,8]
-- Expected Result: 85
-- addAll :: [Int] -> Int
-- ???
addAll :: [Int] -> Int
addAll [] = 0
addAll x = sum x
-- ATAU
-- addAll :: [Int] -> Int
-- addAll xs = sum xs

-- -----------------------------
-- Task 6
-- -----------------------------
-- Pada Result Task 5 di jumlahkan dengan result function checkDigit
-- Run: addWithCheckDigit 85 5
-- Expected Result: 85 + 5 = 90
addWithCheckDigit :: Int -> Int -> Int
addWithCheckDigit x y = x + y

-- -----------------------------
-- Task 7
-- -----------------------------
-- Periksa apakah total penjumlahan tersebut dapat dibagi dengan angka 10
-- Run: isDivisibleby10 90
-- Expected Result: True
-- isDivisibleby10 :: Int -> Bool
-- ???
isDivisibleby10 :: Int -> Bool
isDivisibleby10 x = x `mod` 10 == 0

-- -----------------------------
-- Gabungkan
-- -----------------------------
-- Run: validate [4,5,5,6,7,3,7,5,8,6,8,9,9,8,5,5]
-- Expected Result: True
-- validate :: [Int] -> Bool
-- ???
validate :: [Int] -> Bool
validate [] = False
validate x = 
    let dropIt  = dropLast x
        checkIt = checkDigit x
        reverseIt = reverseList dropIt
        multiplyIt = multiplyOddPlacesby2 reverseIt
        subtractIt = subtract9 multiplyIt
        sumAllIt = addAll subtractIt
        addIt = addWithCheckDigit sumAllIt checkIt
    in  isDivisibleby10 addIt