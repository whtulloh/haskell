-- -----------------------------------------
-- TASK 1
-- -----------------------------------------
-- ghci> let x = 4
-- ghci> let y = x + 3
-- ghci> x * x akan menghasilkan brp ?
-- -----------------------------------------
-- Jawaban
-- -----------------------------------------
-- x * x = 16


-- -----------------------------------------
-- TASK 2
-- -----------------------------------------
-- Perbaiki function dibawah ini agar bisa berjalan tanpa error
kurangi :: Int -> Int -> Int
kurangi g h = g - h


-- -----------------------------------------
-- Task 3
-- -----------------------------------------
-- Perbaiki signature function dibawah ini 
multiplication :: Int -> Int
multiplication  a = 3 * 2


-- -----------------------------------------
-- Task 4
-- -----------------------------------------
bagidua :: Int -> Float
bagidua x = fromIntegral x / 2


-- -----------------------------------------
-- Task 5
-- -----------------------------------------
-- Ada sebuah function sbb
myNestedCond :: Int -> Int -> Int -> Int
myNestedCond a b c = if a <= b
                    then a + 2
                    else
                        if a <= c
                        then a
                        else a - 2
-- sekarang gimana caranya jika angka 2 dan 6 dari code diatas, 
-- ingin ganti dengan variable yg dapat nilai dari beberapa argument, silahkan ubah codenya.


-- -----------------------------------------
-- Task 6
-- -----------------------------------------
-- sekarang bagaimana jika kita ingin keluarannya adalah kata-kata misalnya 
-- Jika lebih kecil dari 2 akan keluar String “lebih kecil dari 2” 
-- jika 6 akan keluar String “lebih kecil dari 6” dan 
-- jika lebih besar dari 6 akan keluar String “lebih besar dari 6”
-- silahkan ubah code diatas agar mempunyai result string seperti yg diinginkan 
mySecondNestedCond :: Int -> Int -> Int -> String
mySecondNestedCond a b c = if a <= b
                    then "lebih kecil atau sama dengan b"
                    else
                        if a <= c
                        then "lebih kecil atau sama dengan a"
                        else "lebih besar dari c"