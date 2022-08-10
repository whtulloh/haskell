-- Add to list
tambahList :: [String] -> IO()
tambahList orderLama = do
                            a <- getLine
                            let b = orderLama ++ [a]
                            print b
-- tambahList ["Jhony"] -> expected: 
-- Rachel -- inputan user
-- ["Jhony","Rachel"]

-- Recursive
data IntList = Empty
                | Cons Int IntList
                deriving Show

intListProduct :: IntList -> Int
intListProduct Empty = 1
intListProduct (Cons head list) = head * intListProduct list
-- intListProduct Empty -> expected: 1
-- intListProduct (Cons 5 Empty) -> expected: 5
-- intListProduct (Cons 5 (Cons 2 Empty)) -> expected: 10
-- intListProduct (Cons 5 (Cons 2 (Cons 3 Empty))) -> expected: 30

-- Tree
data Tree = Leafs Int
            | Nodes Tree Int Tree
            deriving Show

dummyTree :: Tree
dummyTree = Nodes (Leafs 2) 1 (Nodes (Leafs 4) 3 (Leafs 5))
-- dummyTree -> expected: Nodes (Leafs 2) 1 (Nodes (Leafs 4) 3 (Leafs 5))

findInTree :: Int -> Tree -> Bool
findInTree i (Leafs j) = i == j
findInTree i (Nodes left j right) =
                    i == j
                || findInTree i left
                || findInTree i right
-- findInTree 2 dummyTree -> expected: True
-- findInTree 1 dummyTree -> expected: True
-- findInTree 3 dummyTree -> expected: True
-- findInTree 4 dummyTree -> expected: True
-- findInTree 5 dummyTree -> expected: True
-- findInTree 0 dummyTree -> expected: False

-- Logging
data MessageType = Info
                    | Warning
                    | Error Int
                    deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
                deriving (Show, Eq)

cobaParseLog :: String -> LogMessage
cobaParseLog logContents = case (words logContents) of
    "I" : timeStamp : stringMessage -> LogMessage Info (read timeStamp) (unwords stringMessage)
    "W" : timeStamp : stringMessage -> LogMessage Warning (read timeStamp) (unwords stringMessage)
    "E" : errorCode : timeStamp : stringMessage ->
        let arg1 = Error (read errorCode)
            arg2 = read timeStamp
            arg3 = unwords stringMessage
        in LogMessage arg1 arg2 arg3
    stringMessage ->
        Unknown (unwords stringMessage)
-- cobaParseLog "z 12 la la la" -> expected: Unknown "z 12 la la la"
-- cobaParseLog "I 12 la la la" -> expected: LogMessage Info 12 "la la la" 
-- cobaParseLog "W 12 la la la" -> expected: LogMessage Warning 12 "la la la"
-- cobaParseLog "E 12 la la la" -> expected: LogMessage (Error 12) *** Exception: Prelude.read: no parse

-- ghci> words "halo saya rudi" -> expected: ["halo","saya","rudi"]
-- ghci> unwords ["halo","saya","rudi"] -> expected: "halo saya rudi"
-- ghci> :t map -> expected: map :: (a -> b) -> [a] -> [b]
-- ghci> map (+10) [1,2,3,4,5] -> expected: [11,12,13,14,15]
-- ghci> lines "I 42 info\nW 89 warning \nE 100 12 error\nflastjhjhskjkjsd" -> expected: ["I 42 info","W 89 warning ","E 100 12 error","flastjhjhskjkjsd"]

data MessageTree = Leaf 
                    | Node MessageTree LogMessage MessageTree
                    deriving (Show, Eq)

makeLog :: MessageType -> String -> [String] -> LogMessage
makeLog msgType timestamp wordsIntLog = LogMessage msgType (read timestamp) (unwords wordsIntLog)
-- makeLog Info "12" ["read/write problem"] -> expected: LogMessage Info 12 "read/write problem"

parseLog :: String -> [LogMessage]
parseLog rawContents = map parseSingleLog (lines rawContents)
-- parseLog "E 200 90 read/write problem \nI 42 you are doing great" -> expected: [LogMessage (Error 200) 90 "read/write problem",LogMessage Info 42 "you are doing great"]

parseSingleLog :: String -> LogMessage
parseSingleLog str = case words str of
     "I" : timestamp : wordsInLog -> makeLog Info timestamp wordsInLog
     "W" : timestamp : wordsInLog -> makeLog Warning timestamp wordsInLog
     "E" : errorSeverity : timestamp : wordsInLog -> makeLog (Error (read errorSeverity)) timestamp wordsInLog
     _ -> Unknown str
-- parseSingleLog "z 12 la la la" -> expected: Unknown "z 12 la la la"
-- parseSingleLog "I 12 la la la"  -> expected: LogMessage Info 12 "la la la"

-- Membangun sebuah Binary Tree
insert :: LogMessage -> MessageTree -> MessageTree 
insert (Unknown _) tree = tree -- delete Unknown LogMessage
insert log@(LogMessage _ _ _) Leaf = Node Leaf log Leaf
insert log@(LogMessage _ timestamp _) (Node left logInsideNode@(LogMessage _ timestampTree _) right)
   | timestamp < timestampTree = Node  (insert log left) logInsideNode right
   | otherwise = Node left logInsideNode (insert log right)
-- insert (LogMessage Info 12 "la la la") Leaf -> expected: Node Leaf (LogMessage Info 12 "la la la") Leaf
-- insert (LogMessage Info 12 "la la la") (Node Leaf (LogMessage Warning 10 "la la la") Leaf) -> expected: Node Leaf (LogMessage Warning 10 "la la la") (Node Leaf (LogMessage Info 12 "la la la") Leaf)
-- insert (Unknown "z 12 la la la") Leaf -> expected: Leaf
-- insert (Unknown "z 12 la la la") (Node Leaf (LogMessage Warning 10 "la la la") Leaf) -> expected: Node Leaf (LogMessage Warning 10 "la la la") Leaf

-- convert list of logs to a ordered message tree (order by timestamp)
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (log:logs) = insert log (build logs)
-- build [LogMessage Info 12 "la la la"] -> expected: Node Leaf (LogMessage Info 12 "la la la") Leaf
-- build [LogMessage Info 12 "la la la",LogMessage Warning 12 "la la la"] -> expected: Node Leaf (LogMessage Warning 12 "la la la") (Node Leaf (LogMessage Info 12 "la la la") Leaf)

-- Sorting dan dimaksukan ke list
-- get sorted list of log from a tree
-- Expected: tree is already sorted
inOrder:: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = (inOrder left) ++ [root] ++ (inOrder right)
-- inOrder (build [LogMessage Info 12 "la la la",LogMessage Warning 18 "la la la"]) -> expected: [LogMessage Info 12 "la la la",LogMessage Warning 18 "la la la"]
-- inOrder (build [LogMessage Info 12 "la la la",LogMessage Warning 18 "la la la",LogMessage (Error 6) 6 "la la la"]) -> expected: [LogMessage (Error 6) 6 "la la la",LogMessage Info 12 "la la la",LogMessage Warning 18 "la la la"]

-- sortedLogs from list
sortedLogs :: [LogMessage] -> [LogMessage]
sortedLogs logs = inOrder (build logs)
-- sortedLogs [LogMessage Info 12 "la la la",LogMessage Warning 6 "la la la"] -> expected: [LogMessage Warning 6 "la la la",LogMessage Info 12 "la la la"]
-- sortedLogs [LogMessage Info 12 "la la la",LogMessage Warning 18 "la la la",LogMessage (Error 6) 6 "la la la"] -> expected: [LogMessage (Error 6) 6 "la la la",LogMessage Info 12 "la la la",LogMessage Warning 18 "la la la"]

-- get message from log
getMEssageFromLog :: LogMessage -> String
getMEssageFromLog (LogMessage _ _ msg) = msg
getMEssageFromLog (Unknown msg) = msg
-- getMEssageFromLog (LogMessage Info 12 "la la la") -> expected: "la la la"
-- getMEssageFromLog (Unknown "z 12 la la la") -> expected: "z 12 la la la"

-- Memfilter untuk mengambil > 50
isCriticalLog :: LogMessage -> Bool
isCriticalLog (LogMessage (Error severity) _ _) = severity > 50
isCriticalLog _ = False
-- isCriticalLog (LogMessage (Error 51) 200 "error1") -> True
-- isCriticalLog (LogMessage (Error 10) 200 "error1") -> False

-- get error message and add to list
whatWentWrongHelper :: [LogMessage] -> [String]
whatWentWrongHelper logs = map getMEssageFromLog (filter isCriticalLog logs) -- filter menyeleksi query yang bernilai true
-- whatWentWrongHelper [LogMessage Info 98 "info", LogMessage (Error 51) 200 "error1", LogMessage (Error 99) 195 "error2"] -> expected: ["error1","error2"]

-- get error message, sorted, and add to list
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = whatWentWrongHelper (sortedLogs logs)
-- whatWentWrong [LogMessage Info 98 "info", LogMessage (Error 51) 200 "error1", LogMessage (Error 99) 195 "error2"] -> expected ["error2","error1"]

main :: IO ()
main = do 
      let logs = "E 200 90 read/write problem \nI 42 you are doing great\nW 89 warning harddisk almost full\nE 100 12 your disk is full\nflastjhjhskjkjsd\nE 80 10 data overflow"
      let parsedLog = parseLog logs
      print parsedLog
-- main -> expected:
-- [LogMessage (Error 200) 90 "read/write problem",LogMessage Info 42 "you are doing great",LogMessage Warning 89 "warning harddisk almost full",LogMessage (Error 100) 12 "your disk is full",Unknown "flastjhjhskjkjsd",LogMessage (Error 80) 10 "data overflow"]