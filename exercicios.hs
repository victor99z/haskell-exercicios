-- Lista haskell ex. 1 -- Fazer até a questao 11 p/ prova

concatenacao :: [a] -> [a] -> [a]
concatenacao [] ys =
  ys
concatenacao (x : xs) ys =
  x : concatenacao xs ys

pertence :: Eq a => a -> [a] -> Bool
pertence a [] = False
pertence a (x : xs) = x == a || pertence a xs

intersecao :: Eq a => [a] -> [a] -> [a]
intersecao [] [] = []
intersecao a [] = []
intersecao [] b = []
intersecao (x : xs) a =
  if (pertence x a) then x : intersecao xs a else intersecao xs a

inverso :: [a] -> [a]
inverso [] = []
inverso a =
  last a : inverso (init a)

primeiros :: Int -> [a] -> [a]
primeiros n [] = []
primeiros 0 a = [head a]
primeiros n (x : xs) =
  x : primeiros (n -1) xs

ultimos :: Int -> [a] -> [a]
ultimos n [] = []
ultimos 0 a = [last a]
ultimos n a =
  inverso (last a : ultimos (n -1) (init a))

-- TODO: terminar de fazer :/
binParaInt :: String -> Int
binParaInt bin
  | cond x = a
  | cond y = a
  | otherwise = binParaInt a
  where
    a = w x

main :: IO ()
main = do
  putStrLn "Hello"
  print $ fmap fib [0 .. 20]
  print $ fmap (power 2) [0 .. 10]
  print $ sumup [0 .. 5]