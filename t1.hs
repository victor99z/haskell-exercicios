concatenacao :: [a] -> [a] -> [a]
concatenacao as [] = as
concatenacao [] bs = bs
concatenacao (a:as) bs = 
    a : concatenacao as bs

pertence :: Eq a => a -> [a] -> Bool
pertence a [] = False
pertence a (x:xs) = a == x || pertence a xs

intersecao :: Eq a => [a] -> [a] -> [a]
intersecao [] [] = []
intersecao a [] = []
intersecao [] b = []
intersecao (a:as) b = 
    if pertence a b then
        a : intersecao as b
    else
        intersecao as b

inverso :: [a] -> [a]
inverso [] = []
inverso a = 
    last a : inverso(init a)

primeirosn :: Int -> [a] -> [a]
primeirosn 0 a = []
primeirosn n [] = []
primeirosn n (a:as) = 
    a : primeirosn (n-1) as

ultimos :: Int -> [a] -> [a]
ultimos 0 a = []
ultimos n [] = []
ultimos n a =
    (last a) : ultimos (n-1) (init a)

toInt x = read x :: Int
toStr x = show x :: String
toChr x = read x :: Char

fToInt :: Float -> Int
fToInt x = round x

binToInt :: String -> Int
binToInt [] = 0
binToInt (x:xs) = let
        i = ( (toInt [x])*2^( (length (x:xs) ) - 1) )
        r = i + (binToInt xs)
    in r

intToBin :: Int -> String
intToBin 0 = (toStr 0)
intToBin 1 = (toStr 1)
intToBin n = intToBin (div n 2) ++ toStr(mod n 2)


menorValor :: Ord a => [a] -> a
menorValor [x] = x
menorValor (x:xs)
    | xs == [] = x
    | x > (menorValor xs) = (menorValor xs)
    | x < (menorValor xs) = x
    | x == (menorValor xs) = x


removerPrimeiro :: Eq a => [a] -> a -> [a]
removerPrimeiro [] a = []
removerPrimeiro (x:xs) a
    | x == a = removerPrimeiro xs a
    | otherwise = x : removerPrimeiro xs a


ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs)
    | x < (menorValor xs) = x : (ordenar xs)
    | x > (menorValor xs) = (menorValor xs) : (ordenar ( x : (removerPrimeiro xs (menorValor xs))) )
    | otherwise = (ordenar xs)++[x]


dobrar_dir :: (a -> b -> b) -> b -> [a] -> b
dobrar_dir f x [y] = f y x
dobrar_dir f x (y:ys) = f y (dobrar_dir f x ys)

dobrar_esq :: (b -> a -> b) -> b -> [a] -> b
dobrar_esq f x [p] = f x p
dobrar_esq f x y = f (dobrar_esq f x (init y)) (last y)


filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f [] = []
filtrar f (x:xs)
    | (f x) == True = x:filtrar f xs
    | otherwise = filtrar f xs



impar :: Int -> Bool
impar x = mod x 2 /= 0

impares :: [Int] -> [Int]
impares [] = []
impares x = filtrar impar x


mapear :: (a -> b) -> [a] -> [b]
mapear f [] = []
mapear f (x:xs) = (f x) : mapear f xs


tuple_left :: (a,b) -> a
tuple_left x = fst x

primeiros :: [(a, b)] -> [a]
primeiros x = mapear tuple_left x


bool_eq :: Bool -> Bool -> Bool
bool_eq x y = x == y && x == True

todos :: [Bool] -> Bool
todos x = dobrar_dir bool_eq True x

data Tree a = 
    Leaf a 
    | Branch (Tree a) (Tree a)

maior :: Ord a => Tree a -> a
maior (Leaf x) = x
maior (Branch x y)
    | (maior y) < (maior x) = (maior x)
    | otherwise = (maior y)

altura :: Tree a -> Int
altura (Leaf x)=1
altura (Branch x y)
    | (altura y) < (altura x) = 1 + (altura x)
    | otherwise = 1 + (altura y)