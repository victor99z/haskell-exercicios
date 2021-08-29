somaPares :: Int -> Int

somaPares 0 = 0
somaPares n = 
    if (n `mod` 2) == 0 then
        (n-2) + somaPares (n-2)
    else
        somaPares (n-1)