-- Custom data definition
data Color = Red
            | Black
            | Green
            | Blue
            | Pink
            | Yellow
            deriving Show  

lukesLightSaberColor :: Color
lukesLightSaberColor = Green
-- lukesLightSaberColor -> expected Green

listOfColors :: [Color]
listOfColors = [Red, Black]
-- listOfColors -> expected [Red, Black]

warnaLampuMerah :: Color -> Bool
warnaLampuMerah Red = True
warnaLampuMerah Green = True
warnaLampuMerah Yellow = True
warnaLampuMerah Blue = False
warnaLampuMerah _ = False -- _ is wildcard
-- warnaLampuMerah Pink -> expected False
-- warnaLampuMerah Green -> expected True

-- Custom type definition
type Warna = (Color,String) 

warnaKesukaan1 :: Color
warnaKesukaan1 = Red
-- warnaKesukaan1 -> expected Red

warnaKesukaan2 :: Color
warnaKesukaan2 = Green
-- warnaKesukaan2 -> expected Green

warnaKesukaanSiapa :: Warna
warnaKesukaanSiapa = (Red, "Alex")
-- warnaKesukaanSiapa -> expected (Red, "Alex")

-- More variation
tebakWarna :: Warna -> String
tebakWarna (x,y) = "Pemilik warna " ++ show x ++ " adalah " ++ y
-- tebakWarna (Green,"Jhony")-> expected "Pemilik warnaGreenadalah Jhony"

-- Variable double
data FailableDouble = Failure
                        | OK Double
                        deriving Show

ups :: FailableDouble
ups = Failure
-- ups -> expected Failure

oke :: FailableDouble
oke = OK 9.5
-- oke -> expected OK 9.5

-- Other example for double
data CekDoubleKah = BukanDouble | BenarDouble Double deriving Show

safeDiv :: Double -> Double -> CekDoubleKah
safeDiv _ 0 = BukanDouble
safeDiv x y = BenarDouble (x / y)
-- safeDiv 8.5 2.5 -> expected BenarDouble 3.4
-- safeDiv 10 0 -> expected BukanDouble

-- Data type person
data Person = Person String Int Color deriving Show

john :: Person
john = Person "Jhony" 15 Red
-- john -> expected Person "Jhony" 15 Red

getName :: Person -> String
getName (Person x _ _) = x
-- getName john -> expected "Jhony"

getAge :: Person -> Int
getAge (Person _ x _) = x
-- getAge john -> expected 15

getColor :: Person -> Color
getColor (Person _ _ x) = x
-- getColor john -> expected Red

whatIsNameField :: Person -> String
whatIsNameField p@(Person x _ _) = "The name field of (" ++show p++ ") is "++ x
-- whatIsNameField john -> expected  "The name field of (Person \"Jhony\" 15 Red) is Jhony"

-- Mini Application (MENU PIZZA)
data Menu = Menu { menuname :: String, menuprice :: Int} deriving Show

addOrder :: String -> Int -> Int -> Menu
addOrder namamenu jumlah hargasatuan = Menu {menuname = namamenu, menuprice = totalprice}
                                        where totalprice = jumlah * hargasatuan
--  addOrder "Pizza" 2 2000 -> expected Menu {menuname = "Pizza", menuprice = 4000}

--  Other varian addOrder
addOrder2 :: String -> Int -> Int -> Menu
addOrder2 namamenu jumlah hargasatuan = Menu {menuname = namamenu, menuprice = jumlah * hargasatuan}
--  addOrder2 "Pizza" 2 2000 -> expected Menu {menuname = "Pizza", menuprice = 4000}

orderBook :: [Menu] -> String -> Int -> Int -> [Menu]
orderBook oldOrder namamenu jumlah hargasatuan = do 
                                       let orderBaru = Menu {menuname = namamenu, menuprice = jumlah * hargasatuan} 
                                       let newOrderList =  oldOrder ++ [orderBaru]
                                       newOrderList

        
-- Interactive input by User
main :: IO()
main = pesan []

keluar :: [Menu] -> IO()
keluar listOrder = do
        putStrLn "\n========== List pesanan Anda: =========="
        print listOrder
        putStrLn "\n==========    Terima kasih    =========="

pesan :: [Menu] -> IO()
pesan isipesanan = do
        putStrLn "(i) Input Menu"
        putStrLn "(q) Quit/Keluar"
        pilihan <- getLine

        case pilihan of
                "i" -> do
                        putStrLn "\nSilahkan masukan pesanan ? "
                        namamenu <- getLine
                        putStrLn "\nJumlah yang ingin dibeli ? "
                        jumlahmenu <- getLine
                        putStrLn "\nHarga satuan ? "
                        hargasatuan <- getLine
                        let pesanan = orderBook isipesanan namamenu (read jumlahmenu) (read hargasatuan) -- read convert string to integer
                        putStrLn "\n==========   Pesanan Anda adalah   ===========\n"
                        print pesanan
                        putStrLn "\n========== Pesanan sedang diproses ===========\n"
                        pesan pesanan
                _ -> keluar isipesanan
-- main -> expected
-- (i) Input Menu
-- (q) Quit/Keluar
-- i
-- 
-- Silahkan masukan pesanan ? 
-- pizza
-- 
-- Jumlah yang ingin dibeli ?
-- 2
-- 
-- Harga satuan ?
-- 100000
-- 
-- ==========   Pesanan Anda adalah   ===========
-- 
-- [Menu {menuname = "pizza", menuprice = 200000}]
-- 
-- ========== Pesanan sedang diproses ===========
-- 
-- (i) Input Menu
-- (q) Quit/Keluar
-- i
-- 
-- Silahkan masukan pesanan ? 
-- hamburger
-- 
-- Jumlah yang ingin dibeli ?
-- 1
-- 
-- Harga satuan ?
-- 50000
-- 
-- ==========   Pesanan Anda adalah   ===========
-- 
-- [Menu {menuname = "pizza", menuprice = 200000},Menu {menuname = "humberger", menuprice = 50000}]
-- 
-- ========== Pesanan sedang diproses ===========
-- 
-- (i) Input Menu
-- (q) Quit/Keluar
-- q
-- 
-- ========== List pesanan Anda: ==========
-- [Menu {menuname = "pizza", menuprice = 200000},Menu {menuname = "humberger", menuprice = 50000}]
-- 
-- ==========    Terima kasih    ==========