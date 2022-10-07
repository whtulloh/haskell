import Data.Char
import System.IO
-- --------------
-- TASK 1
-- --------------

getPassphrase :: IO (Maybe String)
getPassphrase = do s <- getLine
                   if isValid s then return $ Just s
                                else return Nothing
-- isValid :: String -> Bool
-- isValid s = length s >= 8 && any (`elem` ['0'..'9']) s && any (`elem` ['a'..'z']) s && any (`elem` ['A'..'Z']) s && any (`elem` ['!'..'+']) s
            
isValid :: String -> Bool
isValid s = all (\f -> f s) [checkDigit, checkAlpha, checkPunc, checkLength]
                where checkDigit  = any (isNumber)
                      checkAlpha  = any (isAlpha)
                      checkPunc   = any (isPunctuation)
                      checkLength s = length s >= 8

-- askUserIDPass :: IO ()
-- askUserIDPass = do putStrLn "Insert your new User ID:"
--                    user_id <- getLine
--                    putStrLn "Insert your new password:"
--                    maybe_value <- getPassphrase
--                    case maybe_value of
--                        Just value -> do 
--                                     contents <- readFile "listuserpass.txt"
--                                     length contents `seq` (writeFile "listuserpass.txt" $ user_id ++ " | " ++ contents ++ "\n" ++ value)
--                                     putStrLn "Storing in file..."  
--                        Nothing -> putStrLn "Password invalid."

askUserIDPass :: IO ()
askUserIDPass = do putStrLn "Insert your new User ID:"
                   user_id <- getLine
                   putStrLn "Insert your new password:"
                   maybe_value <- getPassphrase
                   case maybe_value of
                       Just value -> do 
                                    appendFile "listuserpass.txt" $ user_id ++ " | " ++ value ++ "\n"
                                    putStrLn "Storing in file..."  
                       Nothing -> putStrLn "Password invalid."


-- Ubah code diatas agar ketika valid dia akan menyimpan User ID dan password di file listuserpass.txt, dengan ketentuan :
-- 1.	Ubah function askUserIDPass dan isValid saja (jangan menambah function baru)
-- 2.	User harus memasukan user ID dan password untuk disimpan
-- 3.	User ID bebas tanpa ketentuan apapun
-- 4.	Password dianggap valid jika :
-- 	a.	Terdiri dari huruf
-- 	b.	Terdiri dari angka
-- 	c.	Panjangnya lebih atau sama dengan 8
-- 	d.	Dan ada special character apapun
-- 5.	Ketika ada user baru maka file listuserpass.txt akan di append isinya (bukan di overwrite)
-- 6.	Setiap id dan password antara 1 data dan lainnya harus dibuat beda baris (new line)
-- 7.	Silahkan di cari di google atau https://hoogle.haskell.org/
-- 8.	Mungkin bisa dipakai juga library seperti  Data.Char atau System.IO 


-- Expected Result:
-- ghci> askUserIDPass
-- Insert your new User ID:
-- 12345678
-- Insert your new password:
-- Er3456789!!
-- Storing in text file...
