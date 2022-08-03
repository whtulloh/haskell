-- -------------------------------
-- Task 1
-- -------------------------------
-- Buat sebuah function yg akan menambahkan setiap inputan dari user ke dalam list, didepan list yg ada (listnya bebas), contoh listnya adalah [1,2,3].
-- ghci> tambahListDepan 2 [1,2,3]
-- [2,1,2,3]
tambahListDepan :: Int -> [Int] -> [Int]
tambahListDepan a b = a : b


-- -------------------------------
-- Task 2
-- -------------------------------
-- Buat sebuah function yg akan menambahkan setiap inputan dari user ke dalam list, dibelakang list yg ada (listnya bebas), contoh listnya adalah [1,2,3].
-- ghci> tambahListBelakang 10 [1,2,3]
-- [1,2,3,10]
tambahListBelakang :: Int -> [Int] -> [Int]
tambahListBelakang a b = b ++ [a]


-- -------------------------------
-- Task 3
-- -------------------------------
-- Buat sebuah function yg akan mengambil 1 angka didepan sebuah list
-- ekspetasi:
-- ghci>  ambilSatu [5,6,7]
-- 5
ambilSatu :: [Int] -> Int
ambilSatu a = head a


-- -------------------------------
-- Task 4
-- -------------------------------
-- Buat sebuah function yg menggunakan guards dan mengambil 5 elemen list yg diberikan dalam argument,
-- Kondisinya: jika panjang list lebih dari 10 maka mengambil 5 element ,
--                            jika kurang dari 10 maka biarkan saja
-- <10
-- ghci> cekAmbil10 [1,2,3,4,5,6,7,8,9]
-- [1,2,3,4,5,6,7,8,9]
-- >=10
-- ghci> cekAmbil10 [1,2,3,4,5,6,7,8,9,10,12]
-- [1,2,3,4,5]
cekAmbil10 :: [Int] -> [Int]
cekAmbil10 a
                | length a <= 10 = a
                | otherwise = take 5 a


-- -------------------------------
-- Task 5
-- -------------------------------
-- Sama dengan Task 4. Bedanya ini menggunakan pattern Matching dan bisa menambahkan dengan if then else
-- length [1,2,3,4,5] = 5
-- menggunakan: take n [1,2,3,4,5,6,7,8,9,10]
-- menggunakan pattern matching untuk kondisi
cekAmbil10PM :: [Int] -> [Int]
cekAmbil10PM a = if length a <= 10 then a else take 5 a