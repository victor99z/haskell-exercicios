maiorElemento :: Ord a => [a] -> a

maiorElemento [a] = a
maiorElemento (x:xs) =
    if (x > maiorElemento xs) then
        x
    else
        maiorElemento xs