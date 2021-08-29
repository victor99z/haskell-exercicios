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

-- Não é a melhor forma de fazer uma busca binaria.
bsearch :: [Int] -> Int -> Bool
bsearch [] n = False
bsearch xs n =
  -- splitAt :: [int] -> int -> ([int], [int])
  let (as, b : bs) = splitAt (length xs `div` 2) xs
   in if b == n
        then True
        else
          if b > n
            then bsearch as n
            else bsearch bs n

-- Pode usar o let ou o where

main :: IO ()
main = do
  putStrLn "Hello"
  print $ fmap fib [0 .. 20]
  print $ fmap (power 2) [0 .. 10]
  print $ sumup [0 .. 5]