
-- -----------------------------
-- Task 1
-- -----------------------------
-- Function skips
-- To skip Odd value
-- ghci> skips "ABCD"      
-- ["ABCD","BD","C","D"]
-- ghci> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
-- ghci> skips [True,False]       
-- [[True,False],[False]]
-- ghci> skips [1]
-- [[1]]
-- ghci> skips []          
-- []

-- skips :: [a] -> [[a]]
-- skips [] = []
-- skips [a] = [[a]]
-- skips (x:y:xs) 
--             | length (x:y:xs) < 3 = if length (x:y:xs) `mod` 2 == 0 then (x:y:xs) : skips (y:xs) else [y] : skips (y:xs)
--             | length (y:xs) `mod` 2 == 0 = (x:xs) : skips (y:xs)
--             | otherwise = (x:y:xs) : skips (y:xs) 

skip :: Int -> [a] -> [a]
skip n x
    | length x >= n = last (take n x) : skip n (drop n x)
    | otherwise     = []

skips :: [a] -> [[a]]
skips x = map ((flip skip) x) [1..length x]


-- -----------------------------
-- Local Maxima
-- -----------------------------
-- Function localMaxima
-- To get value localMaxima
-- ghci> localMaxima [1,3,2]    
-- [3]
-- ghci> localMaxima [1,2,5,3,4]
-- [5]
-- ghci> localMaxima [1,2,5,3,4,1]
-- [5,4]
-- ghci> localMaxima [1,2,3,4,5]  
-- []

localMaxima :: [Int] -> [Int]
localMaxima [] = []
localMaxima (x:[]) = []
localMaxima (x:y:[]) = []
localMaxima (x:y:z:end)
                | y > z && y > x = y : localMaxima (y : z : end)
                | otherwise = localMaxima (y : z : end)

localMaxima' :: [Integer] -> [Integer]
localMaxima' []       = []
localMaxima' (_:[])   = []
localMaxima' (_:_:[]) = []
localMaxima' (x0:x1:xs)
    | x1 > x0 && x1 > head xs = x1 : localMaxima' (x1:xs)
    | otherwise               = localMaxima' (x1:xs)

-- -----------------------------
-- Task 3 histogram
-- -----------------------------

-- Fungsi dan signature yg digunakan:
-- histogram :: [Int] -> String
-- •	Fungsi ini memerlukan list integers antara 0 dan 9 
-- •	Outputnya adalah histogram yg berbentuk vertical
-- •	Histogram harus menampilkan frekuensi dari elemen yg ada dilist
-- •	Anggap semua elemen adalah antara 0 sampai 9
-- menggunakan functiong main:
-- main = do
--   putStr $ histogram [1,1,1,5]
--   putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
-- Result yang dihasilkan:
-- ghci> histogram [3,5]               
-- "   * *    \n==========\n0123456789\n"
-- ghci> histogram [1,4,5,4,6,6,3,4,2,4,9]
-- "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"
-- ghci> main
--   *
--   *
--   *      *
-- ==========
-- 0123456789
--     *     
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789

histogram :: [Integer] -> String
histogram = plot . count

-- |Return counts of integers from 0 to 9
count :: [Integer] -> [Int]
count = foldl (flip increment) (replicate 10 0)

-- |Add one to a given count
increment :: Integer -> [Int] -> [Int]
increment x counts =
  case splitAt (fromIntegral x) counts of
    (a,n:b) -> a ++ (n+1):b

-- |Plot a vertical bar chart of counts
plot :: [Int] -> String
plot counts =
  unlines $ [drawline h | h <- [maxh,maxh-1..1]] ++ footer
  where drawline level = [if x >= level then '*' else ' ' | x <- counts]
        maxh = maximum counts
        footer = ["==========","0123456789"]

main = do
  putStr $ histogram [1,1,1,5]
  putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
