-- -----------------------------
-- Task 1
-- -----------------------------
-- Data typenya seperti apa yg bisa kita pakai? 
-- silahkan buat type datanya agar bisa dipakai oleh semua function yg didalam task
-- Misalnya ada data seperti ini:
-- NEL 4 [1,2]

data NonEmptyList a = NEL a [a] deriving Show

-- -----------------------------
-- Task 2
-- -----------------------------
-- Silahkan buat function (tanpa maybe) dari type data NonEmptyList agar bisa menghasilkan sbb
-- (dari NEL / NonEmptyList outputnya menjadi list)
-- hasil yg diharapkan
-- ghci> nelToList (NEL 4 [1,2])
-- [4,1,2]
-- ghci> nelToList (NEL 7 [1,2])
-- [7,1,2]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

-- -----------------------------
-- Task 3
-- -----------------------------
-- silahkan terapkan MAYBE didalam type data NonEmptyList tersebut , agar menghasilkan list sbb
-- (dari list biasa menjadi output NEL / NonEmptyList)
-- hasil yg diharapkan
-- ghci> listToNEL [1,2]
-- Just (NEL 1 [2])
-- ghci> listToNEL [4,1,2]
-- Just (NEL 4 [1,2])
-- ghci> listToNEL [7,1,2]
-- Just (NEL 7 [1,2])

listToNEL:: [a] -> Maybe (NonEmptyList a)
listToNEL [] = Nothing
listToNEL (x:xs) = Just (NEL x xs) 

-- -----------------------------
-- Task 4
-- -----------------------------
-- Silahkan buat function tanpa menggunakan Maybe dengan type data NonEmptyList agar bisa mendapatkan head dari list yg diberikan, hingga bs mendapatkan hasill sbb
-- hasil yg diharapkan
-- ghci> headNEL (NEL 2 [1,2])
-- 2
-- ghci> headNEL (NEL 1 [1,2])
-- 1
-- ghci> headNEL (NEL 4 [1,2])
-- 4

headNEL :: NonEmptyList a -> a
headNEL (NEL x _) = x 

-- -----------------------------
-- Task 5
-- -----------------------------
-- Silahkan buat function tanpa menggunakan Maybe dengan type data NonEmptyList agar bisa mendapatkan tail dari list yg diberikan, hingga bs mendapatkan hasill sbb
-- hasil yg diharapkan
-- ghci> tailNEL (NEL 1 [1,2])
-- [1,2]
-- ghci> tailNEL (NEL 2 [1,2])
-- [1,2]

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ xs) = xs