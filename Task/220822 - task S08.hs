
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
skips :: [a] -> [[a]]
skips [] = []
skips [a] = [[a]]
skips (x:y:xs) 
            | length (x:y:xs) < 3 = if length (x:y:xs) `mod` 2 == 0 then (x:y:xs) : skips (y:xs) else [y] : skips (y:xs)
            | length (y:xs) `mod` 2 == 0 = (x:xs) : skips (y:xs)
            | otherwise = (x:y:xs) : skips (y:xs) 


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