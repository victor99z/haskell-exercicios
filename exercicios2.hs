fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

power :: Int -> Int -> Int
power a 0 = 1
power a b = a * power a (b -1)

log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

sumup :: [Int] -> Int
sumup [] = 0
sumup (x : xs) = x + sumup xs

-- bsearch :: [Int] -> Int -> Bool
-- bsearch xs n = False

-- Pode usar o let ou o where