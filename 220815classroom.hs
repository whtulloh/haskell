-- Data Type Maybe
-- Prelude function Maybe
-- data Maybe a = Nothing | Just a deriving (Eq, Ord)

f :: Int -> Maybe Int
f 0 = Nothing
f x = Just x
-- f 0 -> expected: Nothing
-- f 200 -> expected: Just 500

main :: IO ()
main = do
    let value1 = 1
    let value2 = 2
    putStrLn "Result dari value ini adalah: "
    let result1 = maybe False odd (Just value1)
    let result2 = maybe False even (Just value2)
    print("result untuk value 1 ganjil = ", result1)
    print("result untuk value 2 genap = ", result2)
-- main -> expected: 
-- ("result untuk value 1 ganjil = ",True)
-- ("result untuk value 2 = ",True)

tes :: Int -> Maybe Int
tes x = if x>0 then Just x else Nothing

extractAdd :: Maybe Int -> Int
extractAdd Nothing = 0
extractAdd (Just x) = x + 20

cekData :: Int -> IO ()
cekData z = do
            let xx = tes z
            let zz = extractAdd xx
            print("nilainya = "++ show zz)
-- cekData 0 -> expected: "nilainya = 0"
-- cekData 10 -> expected: "nilainya = 30"
-- cekData 20 -> expected: "nilainya = 40"

getHead :: [Int] -> Int
getHead [] = 0
getHead [_] = 0
getHead (x:xs) = x
-- getHead [1,2,3] -> expected: 1
-- getHead [] -> expected: 0
-- getHead [9] -> expected: 0
-- getHead [ ] -> expected: 0
-- getHead [100,200,3] -> expected: 100

doStuff2 :: [Int] -> Int
doStuff2 [] = 0
doStuff2 [_] = 0
doStuff2 (x1:x2:_) = x1 + x2
-- doStuff2 [1,2,3,4,5,6] -> expected: 3
-- doStuff2 [ ] -> expected: 0
-- doStuff2 [2] -> expected: 0

emptyStringList :: [String]
emptyStringList = []

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- main :: IO ()
-- main = print(safeHead emptyStringList, safeHead [1,2,3,4])
-- main -> expected: (Nothing,Just 1)