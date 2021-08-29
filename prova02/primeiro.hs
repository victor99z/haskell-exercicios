impar :: Int -> Bool
impar x = mod x 2 /= 0

primeiro :: (a -> Bool) -> [a] -> Maybe a
primeiro a [] = Nothing
primeiro a (x:xs) 
    | a x = Just x
    | otherwise = primeiro a xs

{-- famoso otherwise == True --}