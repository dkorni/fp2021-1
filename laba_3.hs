-- Лабораторна робота №3
-- студента групи КН-31 підгрупа 1
-- Варіант 9

-- Завдання 1
-- Вставити у список через кожнi n елементiв вказане значення, напр.
-- через n=2 значення ’z’: "1234590"⇒ "12z34z59z0".

-- без вбудованих функцій
insertOwn :: Int -> Char -> String -> String
insertOwn 0 y xs = xs
insertOwn n y [] = []
insertOwn n y xs
 | length xs < n = xs
 | otherwise = take n xs ++ [y] ++ insertOwn n y (drop n xs)

-- Команда: insertOwn 2 'z' "1234590"
-- В результаті дістанемо:
-- "12z34z59z0"

 -- з вбудованими функціями
insert :: Int -> a -> [a] -> [a]
insert n y xs = countdown n xs where
   countdown 0 xs = y:countdown n xs -- reset to original n
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs

-- Команда: insert 2 'z' "1234590"
-- В результаті дістанемо:
-- "12z34z59z0"

-- Завдання 2
-- Знайти перше просте число в указаному дiапазонi.
isPrime :: Integer -> Bool
isPrime n = n > 1 && ( n == 2 ||
   null [ () | i <- [2..n-1], rem n i == 0])

findPrime :: (Integer, Integer) -> Integer
findPrime (a, b) = 
    if isPrime a then a 
    else if not(a==b) 
        then findPrime (a+1, b)
        else 0

-- Команда: findPrime 32 40
-- В результаті дістанемо:
-- 37