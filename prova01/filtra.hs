filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f [] = []
filtrar f (x:xs)
    | (f x == True) = (x : filtrar f xs)
    | otherwise = filtrar f xs

verificaImpar :: Int -> Bool
verificaImpar n = (n `mod` 2) == 1