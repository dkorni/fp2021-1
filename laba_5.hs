-- Лабораторна робота №5
-- студента групи КН-31 підгрупа 1
-- Варіант 9

isPrime :: Int -> Bool
isPrime n = n > 1 && ( n == 2 ||
   null [ () | i <- [2..n-1], rem n i == 0])

findPrime :: (Int, Int) -> Int
findPrime (a, b) = 
    if isPrime a then a 
    else if not(a==b) 
        then findPrime (a+1, b)
        else 0

main :: IO()
main=do
    putStrLn "Enter range start and end:"
    aLine <- getLine
    bLine <- getLine
    let a = read aLine :: Int
    let b = read bLine :: Int
    let c = findPrime(a,b)
    putStrLn $"First prime number is " ++ show c