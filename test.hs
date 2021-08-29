
{-

doubleMe x y = x**2 + y**2

doubleSmallNumber' x = if x > 100
    then x  
    else x*2

let triangles = [(a,b,c) | c <- [1..20], b <- [1..10], a <- [1..10]]



take :: Int -> [a] -> [a]
take 0 xs = [] -- Se o primeiro arg for zero
take n [] = [] -- Se o segundo arg estiver fazio
take n (x:xs) = x : take (n - 1) xs

    Comentario com multiplas linhas
    

import Text.Printf

main = do
    a <- getLine
    b <- getLine
    let x = read a :: Int
    let y = read b :: Int
    let c = x * y
    printf "PROD = %d\n" c


-}
{-
import Debug.trace

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)


main :: IO()
main = do
    let a = fib 3
    let b = fib 3
    print $ a + b
-}


