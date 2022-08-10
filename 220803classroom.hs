-- Function with multiple Argument
sum3 :: Int -> Int -> Int -> Int
sum3 x y z = x + y + z

result :: Int
result = sum3 3 17 8
-- result -> expected: 28


-- Pairs
myNetWorth :: (Int, String)
myNetWorth = (2000, "USD")
-- myNetWorth -> expected: (2000, "USD")

sumPair :: (Int, Int) -> Int
sumPair (a,b) =  a + b
-- sumPair (5,2) -> expected: 7


-- Conditional Case Format
cekNilai1 :: Int -> String
cekNilai1 n = if n < 0 then "Kurang dari nol" else "Lebih dari nol"
-- cekNilai1 5 -> expected: "lebih dari 0"
-- cekNilai1 -1 -> expected: error
-- cekNilai1 (-1) -> expected: "kurang dari 0"


-- Other Conditional Case Format
caseReplaceIf :: Int -> String
caseReplaceIf n =
                case (n < 0) of
                    True -> "Kurang dari nol"
                    False -> "Lebih dari nol"
-- caseReplaceIf 5 -> expected: "lebih dari 0"
-- caseReplaceIf -1 -> expected: error
-- caseReplaceIf (-1) -> expected: "kurang dari 0"


-- Conditional Guard Format
cekNilai2 :: Int -> String
cekNilai2 n
            | n < 0 = "Kurang dari nol"
            | otherwise = "Lebih dari nol"
-- cekNilai2 5 -> expected: "lebih dari 0"
-- cekNilai2 -1 -> expected: error
-- cekNilai2 (-1) -> expected: "kurang dari 0"


-- Conditional Pattern Matching
myPatMatching :: Int -> Int -> Int
myPatMatching 2 3 = 2 + 3
myPatMatching 2 n = 2 + n
myPatMatching _ n = n
-- myPatMatching 2 3 -> expected: 5
-- myPatMatching 2 7 -> expected: 9
-- myPatMatching 100 8 -> expected: 8


-- Conditional with Pattern Matching and Guard
foo :: Int -> Int
foo 0 = 20
foo 1
        | 7 > 5 = 40
foo n
        | n < 0 = 0
        | n `mod` 2 == 0 = 80
        | otherwise = 100
-- foo 0 -> expected: 20
-- foo 1 -> expected: 40
-- foo 7 -> expected: 100
-- foo 8 -> expected: 80
-- foo (-1) -> expected: 0


-- Haskel Char / Array of String
-- ghci> ['s','t','r','o','n','g'] -> expected: "strong"
-- ghci> drop 3 "Hello" -> expected: "lo"


-- Haskel Array
-- ghci> head [1,2,3,4,5] -> expected: 1
-- ghci> tail [1,2,3,4,5] -> expected: [2,3,4,5]
-- ghci> last [1,2,3,4,5] -> expected: 5
-- ghci> [1..10] -> expected: [1,2,3,4,5,6,7,8,9,10]
-- ghci> reverse [1,2,3,4,5] -> expected: [5,4,3,2,1]
-- ghci> drop 3 [1,2,3,4,5] -> expected: [4,5]
-- ghci> sum [1,2,3,4,5] -> expected: 15 -> 1 + 2 + 3 + 4 + 5
-- ghci> product [1,2,3] -> expected: 6 -> 1 * 2 * 3
-- ghci> 7 : [1,2,3,4,5,6] -> expected: [7,1,2,3,4,5,6]
-- ghci> [1,2,3,4,5,6] ++ 7 -> expected: error
-- ghci> [1,2,3,4,5,6] ++ [7] -> expected: [1,2,3,4,5,6,7]
-- ghci> [1,2,3,4,5,6] ++ [7,8,9,10] -> expected: [1,2,3,4,5,6,7,8,9,10]
-- ghci> length [10,20,30,40] -> expected: 4
-- ghci> take 2 [1,2,3,4,5] -> expected: [1,2]
-- ghci> take 1 [1,2,3,4,5] -> expected: [1]
-- ghci> take 4 [1,2,3,4,5] -> expected: [1,2,3,4]


-- List with Pattern Matching
contohPM :: [Int] -> [Int]
contohPM [] = []
contohPM x = if head x == 2 then tail x else x
-- contohPM [1,2,3,4,5] -> expected: [1,2,3,4,5]
-- contohPM [2,3,4,5] -> expected: [3,4,5]

-- Function Recal with Guard
hailstone :: Int -> Int
hailstone n
            | n `mod` 2 == 0 = n `div` 2
            | otherwise = 3 * n + 1
-- hailstone 4 -> expected: 0
-- hailstone 5 -> expected: 16

hailstoneSeq :: Int -> [Int]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)
-- hailstoneSeq 1 -> expected: [1]
-- hailstoneSeq 4 -> expected: [4,2,1]