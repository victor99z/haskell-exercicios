ultimoElemento :: [a] -> a

ultimoElemento (x:[]) = x
ultimoElemento (x:xs) = 
       ultimoElemento xs