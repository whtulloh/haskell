-- Function
huruf :: Int
huruf = 10

jumlahkan1 :: Int -> Int -> Int
jumlahkan1 a b = a + b

kalikan :: Int -> Int -> Int
kalikan a b = a * b + 10

calculator :: Int -> Int -> Int -> Int
calculator a b c = a * b + c


-- Variable Casting
a :: Float
a = 4.5

b :: Int
b = 10

c :: Float
c = a + fromIntegral b


-- Boolean
tidakBenar = False
benar = not tidakBenar
cekKebenaran =  not tidakBenar && benar


-- Char
myChar = 'a'


-- String
myString = "Emurgo"


-- Conditional
condition :: Int -> Int
condition cond = if cond <= 5
                    then cond + 10
                    else 3